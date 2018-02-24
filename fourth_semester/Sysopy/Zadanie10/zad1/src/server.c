#include "server.h"

int main(int argc, char **argv) {
  CHECK_UNEQUAL("main", argc, 3, "Usage: ./server port socket_path")

  atexit(cleanup);
  signal(SIGINT, handle_int);

  strncpy(un_path, argv[2], PATH_MAX - 1);

  in_sfd = create_inet_socket(argv[1]);
  make_socket_non_blocking(in_sfd);
  un_sfd = create_unix_socket(un_path);
  make_socket_non_blocking(un_sfd);

  pthread_create(&net, NULL, net_loop, NULL);
  pthread_create(&console, NULL, console_loop, NULL);
  pthread_create(&pinger, NULL, ping_loop, NULL);
  pthread_join(net, NULL);
  pthread_join(console, NULL);
  pthread_join(pinger, NULL);

  return 0;
}
void cancel_thread(pthread_t thread) {
  if (pthread_cancel(net) == -1)
    perror("Cannot cancel thread");
  if (pthread_join(net, NULL) == -1)
    perror("Cannot join thread");
}
void close_socket(int sfd) {
  if (sfd != -1) {
    if (shutdown(sfd, SHUT_RDWR) == -1)
      perror("Cannot shutdown socket");
    if (close(sfd) == -1)
      perror("Cannot close socket");
  }
}

void cleanup() {
  cancel_thread(net);
  cancel_thread(console);

  println("Time to clean up this mess");
  if (efd != -1 && close(efd) == -1)
    perror("Cannot close epoll file descriptor");

  close_socket(un_sfd);
  close_socket(in_sfd);
  if (unlink(un_path) == -1)
    perror("Cannot unlink socket path");

  pthread_mutex_destroy(&clients_mutex);
}

void handle_int(int s) {
  println("Received SIGINT, closing");
  exit(1);
}

int create_inet_socket(char *port) {
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int sfd;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;     /* Return IPv4 and IPv6 choices */
  hints.ai_socktype = SOCK_STREAM; /* TCP socket */
  hints.ai_flags = AI_PASSIVE;     /* All interfaces */

  /*
    The hints argument points to an addrinfo structure that specifies
    criteria for selecting the socket address structures returned in the
    list pointed to by result. [...]
    Either node or service, but not both, may be NULL.
  */
  CHECK_NON_ZERO("create_socket", getaddrinfo(NULL, port, &hints, &result),
                 "Cannot obtain address info")

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (sfd == -1)
      continue;

    if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0) {
      /* We managed to bind successfully! */
      break;
    }
    CHECK_NULL("create_socket", rp, "Cannot bind")

    close(sfd);
  }

  freeaddrinfo(result);

  return sfd;
}

int create_unix_socket(char *path) {
  int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
  CHECK_NEGATIVE("create_unix_socket", sfd, "Cannot create socket")

  sockaddr_un addr;
  memset(&addr, 0, sizeof(addr));
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, path);

  unlink(path);
  CHECK_NEGATIVE("create_unix_socket",
                 (bind(sfd, (sockaddr *)&addr, sizeof(addr))),
                 "Cannot bind socket");
  return sfd;
}

void make_socket_non_blocking(int sfd) {
  int flags = fcntl(sfd, F_GETFL, 0);
  CHECK_NEGATIVE("create_nonblocking_socket", flags, "fcntl failed")

  flags |= O_NONBLOCK;
  CHECK_NEGATIVE("create_nonblocking_socket", fcntl(sfd, F_SETFL, flags),
                 "fcntl failed")
}

int add_client(char *name, int sfd) {
  pthread_mutex_lock(&clients_mutex);
  if (clients_size >= MAX_CLIENTS) {
    printerr("Client limit reached!");
    pthread_mutex_unlock(&clients_mutex);
    return 0;
  }

  client c;
  strcpy(c.name, name);
  c.sfd = sfd;
  c.pinged = 1;
  c.id = clients_size;
  clients[clients_size++] = c;

  pthread_mutex_unlock(&clients_mutex);
  return 1;
}

void remove_client(int sfd) {
  pthread_mutex_lock(&clients_mutex);
  for (int i = 0; i < clients_size; i++) {
    if (clients[i].sfd == sfd) {
      for (int j = i; j < clients_size - 1; j++) {
        clients[j] = clients[j + 1];
        clients[j].id = j;
      }
      clients_size--;
      break;
    }
  }
  pthread_mutex_unlock(&clients_mutex);
  close(sfd);
  fflush(stdout);
}

int hookup(char *name, int sfd) {
  pthread_mutex_lock(&clients_mutex);
  for (int i = 0; i < clients_size; i++) {
    if (strcmp(clients[i].name, name) == 0) {
      println("Client already exists");
      order ord;
      strcpy(ord.message, "Name already exists");
      ord.type = I_WANNA_DIE;
      if (send(sfd, &ord, sizeof(order), 0) < 0) {
        printerr("Cannot communicate with India");
        close(sfd);
      }
      pthread_mutex_unlock(&clients_mutex);
      return 0;
    }
  }
  pthread_mutex_unlock(&clients_mutex);
  return add_client(name, sfd);
}

int check_epoll_error(epoll_event ev) {
  if ((ev.events & EPOLLERR) || (ev.events & EPOLLHUP) ||
      (ev.events & EPOLLRDHUP)) {
    /* An error has occured on this fd, or the socket is not
       ready for reading
    EPOLLHUP:
       Hang up happened on the associated file descriptor.
    EPOLLERR:
       Error condition happened on the associated file descriptor.
    EPOLLRDHUP:
       Stream socket peer closed connection,
       or shut down writing half of connection
    */
    if ((ev.events & EPOLLRDHUP) || (ev.events & EPOLLHUP)) {
      prompt("Client has closed connection");
    } else {
      fprintf(stderr, "Epoll error\n");
    }
    remove_client(ev.data.fd);
    return 1;
  }
  return 0;
}

int handle_incoming_connection(epoll_event ev) {
  if (ev.data.fd != in_sfd && ev.data.fd != un_sfd) {
    return 0;
  }
  /* We have a notification on the listening socket, which
     means one or more incoming connections. */
  while (1) {
    struct sockaddr in_addr;
    socklen_t in_len;
    int infd;
    char hostbuf[NI_MAXHOST], servbuf[NI_MAXSERV];

    in_len = sizeof(in_addr);
    infd = accept(ev.data.fd, &in_addr, &in_len);
    if (infd == -1) {
      if ((errno == EAGAIN) || (errno == EWOULDBLOCK)) {
        /* We have processed all incoming
           connections. */
        break;
      } else {
        perror("handle_incoming_connection/accept");
        break;
      }
    }

    int s = getnameinfo(&in_addr, in_len, hostbuf, sizeof(hostbuf), servbuf,
                        sizeof(servbuf), NI_NUMERICHOST | NI_NUMERICSERV);
    if (s == 0) {
      print("Accepted connection on descriptor %d "
            "(host=%s, port=%s)",
            infd, hostbuf, servbuf)
    }

    /* Make the incoming socket non-blocking and add it to the
       list of fds to monitor. */
    make_socket_non_blocking(infd);

    epoll_event event;
    event.data.fd = infd;
    event.events = EPOLLIN | EPOLLET;
    CHECK_NEGATIVE("handle_incoming_connection",
                   epoll_ctl(efd, EPOLL_CTL_ADD, infd, &event),
                   "Cannot add epoll event")
  }

  return 1;
}

void handle_communication(epoll_event ev) {
  /* We have data on the fd waiting to be read. Read and
     display it. We must read whatever data is available
     completely, as we are running in edge-triggered mode
     and won't get a notification again for the same
     data. */
  int done = 0;

  while (!done) {
    order ord;

    /*
      MSG_WAITALL requests that the operation block until the full
      request is satisfied.  However, the call may still return less
      data than requested if a signal is caught, an error or
      disconnect occurs, or the next data to be received is of a
      different type than that returned.
    */
    ssize_t count = recv(ev.data.fd, &ord, sizeof(order), MSG_WAITALL);

    if (count == -1) {
      /* If errno == EAGAIN, that means we have read all
         data. So go back to the main loop. */
      if (errno != EAGAIN) {
        perror("handle_communication/recv");
        remove_client(ev.data.fd);
      }
      done = 1;
    } else if (count == 0) {
      /* End of file. The remote has closed the
         connection. */
      done = 1;
      prompt("Client closed the connection");
      remove_client(ev.data.fd);
    } else if (count < sizeof(order)) {
      printerr("Did not read whole message")
    } else {
      switch (ord.type) {
      case I_WANNA_DIE:
        remove_client(ev.data.fd);
        break;
      case CALCULATE:
        printf("Order %d: %d %c %d = %d %s", ord.id, ord.data.x, ord.data.op,
               ord.data.y, ord.data.result, ord.message);
        prompt("");
        break;
      case HI_THERE:
        if (!hookup(ord.message, ev.data.fd)) {
          done = 1;
        }
        break;
      case PONG:
        handle_pong(ord);
        break;
      default:
        prompt("Something is wrong with you, client");
        break;
      }
    }
  }
}

void outsource(int x, int y, char operator, int *order_count) {
  if (clients_size < 1) {
    prompt("You need to hire more Indians");
    return;
  }

  order ord;
  operation opera = {x, y, operator, 0 };
  ord.data = opera;
  ord.message[0] = '\0';
  ord.type = CALCULATE;
  ord.id = *order_count;
  *order_count += 1;

  client c = clients[rand() % clients_size];
  print("%s, tell me what is %d %c %d known as order %d", c.name, x, operator,
        y, *order_count - 1);
  if (send(c.sfd, &ord, sizeof(order), 0) < 0) {
    printerr("Cannot communicate with India");
    prompt("")
  }
}

void handle_pong(order ord) {
  int id;
  sscanf(ord.message, "%d", &id);
  pthread_mutex_lock(&clients_mutex);
  clients[id].pinged = 1;
  pthread_mutex_unlock(&clients_mutex);
}

void *net_loop(void *data) {
  CHECK_NEGATIVE("main", listen(in_sfd, SOMAXCONN), "Listening failed")
  CHECK_NEGATIVE("main", listen(un_sfd, SOMAXCONN), "Listening failed")

  CHECK_NEGATIVE("main", (efd = epoll_create1(0)), "Failed to create an epoll")

  epoll_event event;
  event.data.fd = in_sfd;
  event.events = EPOLLIN | EPOLLET;
  CHECK_NEGATIVE("net_loop/in_sfd",
                 epoll_ctl(efd, EPOLL_CTL_ADD, in_sfd, &event),
                 "Cannot add epoll event")
  event.data.fd = un_sfd;
  CHECK_NEGATIVE("net_loop/un_sfd",
                 epoll_ctl(efd, EPOLL_CTL_ADD, un_sfd, &event),
                 "Cannot add epoll event")

  epoll_event events[MAXEVENTS] = {};
  while (1) {
    int n = epoll_wait(efd, events, MAXEVENTS, -1);
    CHECK_NEGATIVE("net_loop", n, "Error while waiting for events")

    for (int i = 0; i < n; i++) {
      if (check_epoll_error(events[i])) {
        continue;
      }
      if (handle_incoming_connection(events[i])) {
        prompt("New connection estabilished");
        continue;
      }
      handle_communication(events[i]);
    }
  }

  free(events);
  return 0;
}

void *console_loop(void *data) {
  prompt("Enter var1 var2 operator");
  int order_count = 0;

  while (1) {
    int x = 0, y = 0;
    char operator= '+';

    if (scanf("%d %d %c", &x, &y, &operator) == EOF) {
      break;
    }
    __fpurge(stdin);

    outsource(x, y, operator, &order_count);
  }
  return 0;
}

void *ping_loop(void *data) {
  while (1) {
    pthread_mutex_lock(&clients_mutex);
    for (int i = 0; i < clients_size; i++) {
      client *c = clients + i;

      if (!c->pinged) {
        print("%s/%d: PANG", c->name, c->sfd);
        prompt("");
        pthread_mutex_unlock(&clients_mutex);
        remove_client(c->sfd);
        pthread_mutex_lock(&clients_mutex);
        continue;
      }

      c->pinged = 0;
      order ord;
      ord.type = PING;
      sprintf(ord.message, "%d", i);
      if (send(c->sfd, &ord, sizeof(order), 0) < 0) {
        printerr("Cannot communicate with India");
        remove_client(c->sfd);
      }
    }
    pthread_mutex_unlock(&clients_mutex);

    sleep(3);
  }
  return NULL;
}

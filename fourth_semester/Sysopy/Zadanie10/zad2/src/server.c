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
  hints.ai_family = AF_UNSPEC;    /* Return IPv4 and IPv6 choices */
  hints.ai_socktype = SOCK_DGRAM; /* UDP socket */
  hints.ai_flags = AI_PASSIVE;    /* All interfaces */

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
  int sfd = socket(AF_UNIX, SOCK_DGRAM, 0);
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

int add_client(char *name, int sfd, sockaddr_wtv addr) {
  pthread_mutex_lock(&clients_mutex);
  if (clients_size >= MAX_CLIENTS) {
    ERROR_NE("add_client", "Client limit reached!");
    pthread_mutex_unlock(&clients_mutex);
    return 0;
  }

  client c;
  strcpy(c.name, name);
  c.sfd = sfd;
  c.pinged = 1;
  c.id = clients_size;
  c.addr = addr;

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
  print("rmv > %d", clients_size);
}

int hookup(char *name, int sfd, sockaddr_wtv addr) {
  pthread_mutex_lock(&clients_mutex);
  for (int i = 0; i < clients_size; i++) {
    if (strcmp(clients[i].name, name) == 0) {
      println("Client already exists");
      order ord;
      strcpy(ord.message, "Name already exists");
      ord.type = I_WANNA_DIE;

      sockaddr *sa = (sockaddr *)&addr;
      socklen_t size =
          sa->sa_family == AF_UNIX ? sizeof(sockaddr_un) : sizeof(sockaddr_in);

      if (sendto(sfd, &ord, sizeof(order), 0, (sockaddr *)&addr, size) < 0) {
        ERROR_NE("hookup", "Cannot communicate with India");
        close_socket(sfd);
      }
      pthread_mutex_unlock(&clients_mutex);
      return 0;
    }
  }
  pthread_mutex_unlock(&clients_mutex);
  return add_client(name, sfd, addr);
}

int check_epoll_error(epoll_event ev) {
  if ((ev.events & EPOLLERR) || (ev.events & EPOLLHUP)) {
    /* An error has occured on this fd, or the socket is not
       ready for reading
    EPOLLHUP:
       Hang up happened on the associated file descriptor.
    EPOLLERR:
       Error condition happened on the associated file descriptor.
    */
    if ((ev.events & EPOLLHUP)) {
      prompt("Client has closed connection");
    } else {
      fprintf(stderr, "Epoll error\n");
    }
    remove_client(ev.data.fd);
    return 1;
  }
  return 0;
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
    sockaddr_wtv addr;
    socklen_t addrlen = sizeof(addr);

    /*
      MSG_WAITALL requests that the operation block until the full
      request is satisfied.  However, the call may still return less
      data than requested if a signal is caught, an error or
      disconnect occurs, or the next data to be received is of a
      different type than that returned.
    */
    ssize_t count = recvfrom(ev.data.fd, &ord, sizeof(order), MSG_WAITALL,
                             (sockaddr *)&addr, &addrlen);

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
      ERROR_NE("handle_communication", "Did not read whole message")
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
        if (!hookup(ord.message, ev.data.fd, addr)) {
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

  sockaddr *sa = (sockaddr *)&c.addr;
  socklen_t size =
      sa->sa_family == AF_UNIX ? sizeof(sockaddr_un) : sizeof(sockaddr_in);

  if (sendto(c.sfd, &ord, sizeof(order), 0, (sockaddr *)&c.addr, size) < 0) {
    ERROR_NE("outsource", "Cannot communicate with India");
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

      sockaddr *sa = (sockaddr *)&c->addr;
      socklen_t size =
          sa->sa_family == AF_UNIX ? sizeof(sockaddr_un) : sizeof(sockaddr_in);

      if (sendto(c->sfd, &ord, sizeof(order), 0, (sockaddr *)&c->addr, size) <
          0) {
        ERROR_NE("ping", "Cannot communicate with India")
        remove_client(c->sfd);
      }
    }
    pthread_mutex_unlock(&clients_mutex);

    sleep(3);
  }
  return NULL;
}

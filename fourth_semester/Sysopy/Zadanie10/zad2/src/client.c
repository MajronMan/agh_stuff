#include "client.h"

sockaddr_wtv serv_addr;

int main(int argc, char **argv) {
  char name[256];
  char address[256];
  int type;
  short port;
  parse_args(argc, argv, name, &type, address, &port);

  atexit(cleanup);
  signal(SIGINT, handle_int);

  my_socket =
      type ? create_unix_socket(address) : create_inet_socket(port, address);

  hookup(name);

  eternal_struggle();
  return 0;
}

void parse_args(int argc, char **argv, char *name, int *type, char *address,
                short *port) {
  if (argc != 4) {
    printf("usage: %s client_name socket_type IP:port | path\n", argv[0]);
    exit(10);
  }
  strcpy(name, argv[1]);
  if (strcmp(argv[2], "unix") == 0) {
    *type = 1;
    strcpy(address, strtok(argv[3], ":"));
    if (strtok(NULL, ":") != NULL) {
      println("unix socket requires path, not IP");
      exit(1);
    }
  } else if (strcmp(argv[2], "inet") == 0) {
    *type = 0;
    strcpy(address, strtok(argv[3], ":"));
    char *cport = strtok(NULL, ":");
    if (cport == NULL) {
      println("inet socket requires ip address in form xxx.xxx.xxx.xxx:yyyy");
      exit(1);
    }
    *port = atoi(cport);
  } else {
    println("socket type must be unix or inet");
    exit(1);
  }
}

int create_inet_socket(short port, char *ip) {
  int sfd = socket(AF_INET, SOCK_DGRAM, 0);
  CHECK_NEGATIVE("create_inet_socket", sfd, "Cannot create socket")

  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);

  CHECK_ZERO("create_inet_socket", inet_aton(ip, &addr.sin_addr),
             "Invalid IP address")

  CHECK_NEGATIVE("create_inet_socket",
                 connect(sfd, (const struct sockaddr *)&addr, sizeof(addr)),
                 "Cannot connect to socket")

  serv_addr.sin = addr;
  return sfd;
}

void create_unix_path(char *destination) {
  char path[256];
  strcpy(path, CLIENT_SOCKET_PATH);
  char num[50];
  sprintf(num, "%d%d", getpid(), rand());
  strcat(path, num);
  print("my unix path: %s", path);
  strcpy(destination, path);
}

int create_unix_socket(char *path) {
  int sfd = socket(AF_UNIX, SOCK_DGRAM, 0);
  CHECK_NEGATIVE("create_unix_socket", sfd, "Cannot create socket")

  sockaddr_un caddr;
  sockaddr_un saddr;
  memset(&caddr, 0, sizeof(caddr));
  memset(&saddr, 0, sizeof(saddr));
  caddr.sun_family = AF_UNIX;
  saddr.sun_family = AF_UNIX;
  create_unix_path(caddr.sun_path);
  strcpy(saddr.sun_path, path);

  unlink(caddr.sun_path);
  CHECK_NEGATIVE("create_unix_socket",
                 (bind(sfd, (sockaddr *)&caddr, sizeof(caddr))),
                 "Cannot bind socket");

  CHECK_NEGATIVE("create_unix_socket",
                 connect(sfd, (const struct sockaddr *)&saddr, sizeof(saddr)),
                 "failed connecting unix socket")

  serv_addr.sun = saddr;
  return sfd;
}

void fulfill_my_purpose(order *ord) {
  switch (ord->data.op) {
  case '+':
    ord->data.result = ord->data.x + ord->data.y;
    break;
  case '-':
    ord->data.result = ord->data.x - ord->data.y;
    break;
  case '*':
    ord->data.result = ord->data.x * ord->data.y;
    break;
  case '/':
    if (ord->data.y == 0) {
      ord->data.result = INT_MAX;
      strcpy(ord->message, "Indians cannot divide by 0");
    } else {
      ord->data.result = ord->data.x / ord->data.y;
    }
    break;
  default:
    strcpy(ord->message, "Indians only know how to + - * /");
    break;
  }
  CHECK_NEGATIVE("fulfill_my_purpose", send(my_socket, ord, sizeof(order), 0),
                 "My life is pointless")
}

void hookup(char *name) {
  println("Saying hello");
  order ord;
  ord.type = HI_THERE;
  strcpy(ord.message, name);
  CHECK_NEGATIVE("hookup", send(my_socket, &ord, sizeof(order), 0),
                 "Cannot say hello :(")
}

void play(order ord) {
  ord.type = PONG;
  CHECK_NEGATIVE("play", send(my_socket, &ord, sizeof(order), 0),
                 "Cannot play ping-pong :(")
}

void eternal_struggle() {
  while (1) {
    order ord;
    ssize_t count = recv(my_socket, &ord, sizeof(order), MSG_WAITALL);
    if (count == -1 && errno != EAGAIN) {
      ERROR("eternal_struggle", "I cannot hear anymore")
    } else if (count == 0) {
      println("Finally free");
      return;
    } else if (count < sizeof(order)) {
      println("This one goes into the void");
    } else {
      switch (ord.type) {
      case CALCULATE:
        println("Time to fulfill my purpose");
        fulfill_my_purpose(&ord);
        break;
      case PING:
        printf(".");
        fflush(stdout);
        play(ord);
        break;
      case I_WANNA_DIE:
        print("%s", ord.message);
        return;
      default:
        println("Leave me alone!");
        break;
      }
    }
  }
}

void cleanup() {
  // if (my_socket > -1) {
  //   if (shutdown(my_socket, SHUT_RDWR) < 0) {
  //     fprintf(stderr, "Cleanup could not shutdown socket\n");
  //   }
  //   if (close(my_socket) < 0) {
  //     fprintf(stderr, "Cleanup could not close socket\n");
  //   }
  // }
}

void handle_int(int sig) {
  println("Received SIGINT: shutting down");
  exit(1);
}

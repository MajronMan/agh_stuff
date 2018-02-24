#ifndef COMMON_H
#define COMMON_H

#define _DEFAULT_SOURCE

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

#ifndef NI_MAXHOST
#define NI_MAXHOST 1025
#endif
#ifndef NI_MAXSERV
#define NI_MAXSERV 32
#endif

#define MESSAGE_LENGTH 256
#define CLIENT_NAME_SIZE 256

typedef struct sockaddr_un sockaddr_un;
typedef struct epoll_event epoll_event;
typedef struct sockaddr_in sockaddr_in;
typedef struct sockaddr sockaddr;

typedef union sockaddr_wtv {
  struct sockaddr sa;
  struct sockaddr_in sin;
  struct sockaddr_un sun;
} sockaddr_wtv;

typedef struct client {
  char name[CLIENT_NAME_SIZE];
  int sfd;
  int id;
  char pinged;
  sockaddr_wtv addr;
} client;

typedef struct operation {
  int x;
  int y;
  char op;
  int result;
} operation;

typedef struct order {
  char type;
  operation data;
  char message[MESSAGE_LENGTH];
  int id;
} order;

typedef enum actions { I_WANNA_DIE, CALCULATE, PING, PONG, HI_THERE } actions;

#define ERROR_NE(FUN, ERR)                                                     \
  char msg[100];                                                               \
  sprintf(msg, "%s: %s", FUN, ERR);                                            \
  perror(msg);

#define ERROR(FUN, ERR)                                                        \
  ERROR_NE(FUN, ERR)                                                           \
  exit(-1);

#define CHECK(FUN, VAL, COMP, ERR)                                             \
  if ((VAL) == COMP) {                                                         \
    ERROR(FUN, ERR)                                                            \
  }
#define CHECK_UNEQUAL(FUN, VAL, COMP, ERR)                                     \
  if ((VAL) != COMP) {                                                         \
    ERROR(FUN, ERR)                                                            \
  }
#define CHECK_NEGATIVE(FUN, VAL, ERR)                                          \
  if (VAL < 0) {                                                               \
    ERROR(FUN, ERR)                                                            \
  }

#define CHECK_NON_ZERO(FUN, VAL, ERR) CHECK_UNEQUAL(FUN, VAL, 0, ERR)
#define CHECK_ZERO(FUN, VAL, ERR) CHECK(FUN, VAL, 0, ERR)
#define CHECK_NULL(FUN, VAL, ERR) CHECK(FUN, VAL, NULL, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

#define println(STR)                                                           \
  printf(STR "\n");                                                            \
  fflush(stdout);
#define print(FORMAT, args...)                                                 \
  printf(FORMAT "\n", args);                                                   \
  fflush(stdout);

#define prompt(STR)                                                            \
  printf(STR "\n$#> ");                                                        \
  fflush(stdout);

#endif /* end of include guard: COMMON_H */

#ifndef SERVER_H
#define SERVER_H
#include "common.h"
#include <stdio_ext.h>

typedef struct socket_data1 {
  char *port;
  char *socket_path;
} socket_data1;

typedef struct socket_data {
  int sfd;
  int efd;
  epoll_event *event;
} socket_data;

#define MAX_CLIENTS 20
#define MAX_EVENTS 100

client clients[MAX_CLIENTS] = {};
int clients_size = 0;
pthread_mutex_t clients_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_t net;
pthread_t console;
pthread_t pinger;
int efd;
int un_sfd = -1;
int in_sfd = -1;
char un_path[PATH_MAX] = {};

void cancel_thread(pthread_t thread);
void cleanup();

void handle_int(int s);

void close_socket(int sfd);
int create_inet_socket(char *port);
int create_unix_socket(char *path);
void make_socket_non_blocking(int sfd);

int add_client(char *name, int sfd, sockaddr_wtv addr);
void remove_client(int sfd);

int check_epoll_error(epoll_event ev);
int hookup(char *name, int sfd, sockaddr_wtv addr);
void read_data(epoll_event ev);
void outsource(int x, int y, char op, int *order_count);
void handle_pong(order ord);

void *net_loop(void *data);
void *console_loop(void *data);
void *ping_loop(void *data);

#define MAXEVENTS 64
#endif /* end of include guard: SERVER_H */

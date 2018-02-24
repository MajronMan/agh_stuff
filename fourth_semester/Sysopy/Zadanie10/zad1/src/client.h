#ifndef CLIENT_H
#define CLIENT_H
#include "common.h"

int my_socket = -1;

void parse_args(int argc, char **argv, char *name, int *type, char *address,
                short *port);

int create_inet_socket(short port, char *ip);
int create_unix_socket(char *path);

void fulfill_my_purpose(order *ord);
void hookup(char *name);
void play(order ord);
void eternal_struggle();

void cleanup();
void handle_int(int sig);
#endif /* end of include guard: CLIENT_H */

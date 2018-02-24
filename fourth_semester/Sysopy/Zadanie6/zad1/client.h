#ifndef ZADANIE6_CLIENT_H
#define ZADANIE6_CLIENT_H

#include "common.h"

char doexit = 0;
key_t key;
int my_qid, server_qid;
int id;

int sayHello();
char handle_command(char *cmd);
#endif //ZADANIE6_CLIENT_H

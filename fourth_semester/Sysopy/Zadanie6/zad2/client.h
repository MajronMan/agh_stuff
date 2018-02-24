#ifndef ZADANIE6_CLIENT_H
#define ZADANIE6_CLIENT_H

#include "common.h"

#define NAME_LEN 15

char doexit = 0;
key_t key;
int my_qid, server_qid;
int id;
char q_name[NAME_LEN + 1];

char *gen_random_name();

int sayHello();

char handle_command(char *cmd);

#endif //ZADANIE6_CLIENT_H
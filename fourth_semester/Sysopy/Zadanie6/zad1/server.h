#ifndef ZADANIE6_SERVER_H
#define ZADANIE6_SERVER_H

#include "common.h"

#define MAX_CLIENTS 1000

int clients[MAX_CLIENTS] = {};
unsigned client_count = 0;
char doexit = 0;
int qid;
msg shutdownMessage = {DIE, "Server shutting down", 0};

void remove_queue();

void handle(msg *pMsg);

void hello(int qid);

void bye(int id);

void echo(int id, char *what);

void upper(int id, char *what);

void what_time_is_it(int id);

int get_next_client_id();

#endif //ZADANIE6_SERVER_H

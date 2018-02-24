#ifndef MAIN_H
#define MAIN_H
#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <pthread.h>
#include <signal.h>

static pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;
static int fd;
static int records_count;
static char *seeked_word;
static pthread_t *thread_ids;
static int thread_count;

void *seek(void *arg);

#define wait(S, MS)     struct timeval tv; \
  tv.tv_sec = S; \
  tv.tv_usec = MS; \
  select(0, NULL, NULL, NULL, &tv);


#endif /* end of include guard: MAIN_H */

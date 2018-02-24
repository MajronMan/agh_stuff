#ifndef MAIN_H
#define MAIN_H

#include <stdio.h>
#include <pthread.h>

static pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;
static int fd;
static int records_count;
static char *seeked_word;
static pthread_t *thread_ids;
static int thread_count;

void *seek1(void *arg);
void *seek2(void *arg);
void *seek3(void *arg);

#define wait(MS)     struct timeval tv; \
  tv.tv_sec = 0; \
  tv.tv_usec = MS; \
  select(0, NULL, NULL, NULL, &tv);


#endif /* end of include guard: MAIN_H */

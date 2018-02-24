#ifndef COMMON_H
#define COMMON_H

#define _XOPEN_SOURCE 700
#include <signal.h>
#include <sys/time.h>
#include <sys/select.h>
#include <fcntl.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <unistd.h>
#include <sys/wait.h>

#define SHARED_Q_NAME "waitRoom"
#define PILLOW_SEM_NAME "pillow"
#define QUEUE_SEM_NAME "queueSem"
#define SEAT_SEM_NAME "seat"

#define ERROR(FUN, ERR)char msg[100]; \
  sprintf(msg, "%s: %s\n", FUN, ERR); \
  perror(msg); \
  exit(-1); \

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {ERROR(FUN, ERR)}
#define CHECK_UNEQUAL(FUN, VAL, COMP, ERR)if((VAL) != COMP) {ERROR(FUN, ERR)}
#define CHECK_NEGATIVE(FUN, VAL, ERR) if(VAL < 0) {ERROR(FUN, ERR)}

#define CHECK_NON_ZERO(FUN, VAL, ERR) CHECK_UNEQUAL(FUN, VAL, 0, ERR)
#define CHECK_ZERO(FUN, VAL, ERR) CHECK(FUN, VAL, 0, ERR)
#define CHECK_NULL(FUN, VAL, ERR) CHECK(FUN, VAL, NULL, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)
#define CREATE_SEMAPHORE(ID, NAME) ID = sem_open(NAME, O_CREAT | O_RDWR, 0666, 1); \
  CHECK("create_semaphore", ID, SEM_FAILED, "Cannot obtain semaphore");
#define GET_SEMAPHORE(ID, NAME) ID = sem_open(NAME, O_RDWR); \
  CHECK("get_semaphore", ID, SEM_FAILED, "Cannot obtain semaphore");

void prolaag(sem_t *semaphore) {
  CHECK_NEGATIVE("prolaag", sem_wait(semaphore), "Cannot take semaphore")
}

void verhoog(sem_t *semaphore) {
  CHECK_NEGATIVE("verhoog", sem_post(semaphore), "Cannot give semaphore")
}

long get_timestamp() {
  struct timespec timestamp;
  CHECK_NEGATIVE_ONE("get_timestamp",
                     clock_gettime(CLOCK_MONOTONIC, &timestamp), "Cannot obtain timestamp")
  return (timestamp.tv_sec * 1000000 + timestamp.tv_nsec / 1000);
}

char is_sem_taken(sem_t *semaphore){
  int res;
  sem_getvalue(semaphore, &res);
  return (char) res;
}


#endif

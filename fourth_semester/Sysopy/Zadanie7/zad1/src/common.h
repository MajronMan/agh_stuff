#ifndef COMMON_H
#define COMMON_H

#define _XOPEN_SOURCE 700
#include <signal.h>
#include <sys/time.h>
#include <sys/select.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <unistd.h>
#include <sys/wait.h>

typedef struct sembuf sembuf;

#define PILLOW_SEM_KEYGEN 1
#define SEAT_SEM_KEYGEN 2
#define QUEUE_SEM_KEYGEN 3
#define QUEUE_MEM_KEYGEN 4

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
#define CREATE_SEMAPHORE(ID, KEYGEN) ID = semget(ftok("/home", KEYGEN), \
                                                 1, IPC_CREAT | 0666); \
  semctl(ID, 0, SETVAL, 1); \
  CHECK_NEGATIVE("get_semaphore", ID, "Cannot obtain semaphore")
#define GET_SEMAPHORE(ID, KEYGEN) ID = semget(ftok("/home", KEYGEN), 0, 0); \
  CHECK_NEGATIVE("get_semaphore", ID, "Cannot obtain semaphore")

void change_sem(int sem_id, sembuf *semaphore, char down) {
  semaphore->sem_num = 0;
  semaphore->sem_op = (short) (1 - 2 * down);
  semaphore->sem_flg = 0;
  CHECK_NEGATIVE("change_sem", semop(sem_id, semaphore, 1), "Cannot modify semaphore")
}

void prolaag(int sem_id, sembuf *semaphore) {
  change_sem(sem_id, semaphore, 1);
}

void verhoog(int sem_id, sembuf *semaphore) {
  change_sem(sem_id, semaphore, 0);
}

long get_timestamp() {
  struct timespec timestamp;
  CHECK_NEGATIVE_ONE("get_timestamp",
                     clock_gettime(CLOCK_MONOTONIC, &timestamp), "Cannot obtain timestamp")
  return (timestamp.tv_sec * 1000000 + timestamp.tv_nsec / 1000);
}

char is_sem_taken(int sem_id){
  return (char) semctl(sem_id, 0, GETVAL);
}


#endif

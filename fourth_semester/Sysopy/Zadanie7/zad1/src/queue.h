#ifndef QUEUE_H
#define QUEUE_H

#include <unistd.h>
#include <sys/sem.h>
#define QUEUE_CAPACITY 512

typedef struct queue {
  unsigned int size;
  unsigned int start;
  unsigned int capacity;
  pid_t array[QUEUE_CAPACITY];
  pid_t seat;
  pid_t pillow;
} queue;

char is_empty(queue *queue){
    return queue->size==0;
}

pid_t pop(queue *queue){
    if(is_empty(queue)) return -1;

    unsigned int start = queue->start;
    queue->start = (start + 1) % queue->capacity;
    queue->size--;
    return queue->array[start];
}

char push(queue *queue, pid_t elem){
    if(queue->size >= queue->capacity) return 0;

    queue->array[(queue->start + queue->size++) % queue->capacity] = elem;
    return 1;
}

#define NEW_SHARED_Q(Q, ID, KEYGEN, CAP)if(CAP>QUEUE_CAPACITY) \
  { ERROR("new_shared_q", "Cannot create such a big queue") } \
  ID = shmget(ftok("/home", KEYGEN), sizeof(queue*), IPC_CREAT | 0666); \
  CHECK_NEGATIVE("new_shared_q", ID, "Cannot obtain shared queue") \
  Q = (queue*) shmat(ID, NULL, 0); \
  Q->size = 0; Q->start = 0; Q->capacity = CAP;

#define GET_SHARED_Q(Q, ID, KEYGEN) ID = shmget(ftok("/home", KEYGEN), 0, 0); \
  CHECK_NEGATIVE("get_shared_q", ID, "Cannot obtain shared queue") \
  Q = (queue*) shmat(ID, NULL, 0);

#endif

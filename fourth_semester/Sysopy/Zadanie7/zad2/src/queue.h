#ifndef QUEUE_H
#define QUEUE_H

#include <unistd.h>
#include <sys/sem.h>
#include <fcntl.h>
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

#define NEW_SHARED_Q(Q, ID, NAME, CAP)if(CAP>QUEUE_CAPACITY) \
  { ERROR("new_shared_q", "Cannot create such a big queue") } \
  ID = shm_open(NAME, O_CREAT | O_RDWR, 0666); \
  CHECK_NEGATIVE("new_shared_q", ID, "Cannot obtain shared queue"); \
  CHECK_NEGATIVE("new_shared_q", ftruncate(ID, sizeof(queue)), "Cannot set memory size"); \
  void *sq = mmap(NULL, sizeof(queue), PROT_READ | PROT_WRITE, MAP_SHARED, ID, 0); \
  CHECK_NEGATIVE("new_shared_q", sq, "Cannot map memory"); \
  Q = (queue*) sq; \
  Q->size = 0; Q->start = 0; Q->capacity = CAP;

#define GET_SHARED_Q(Q, ID, NAME) ID =  shm_open(NAME, O_RDWR, 0666); \
  CHECK_NEGATIVE("get_shared_q", ID, "Cannot obtain shared queue") \
  void *sq = mmap(NULL, sizeof(queue), PROT_READ | PROT_WRITE, MAP_SHARED, ID, 0); \
  CHECK_NEGATIVE("new_shared_q", sq, "Cannot map memory"); \
  Q = (queue*) sq;

#endif

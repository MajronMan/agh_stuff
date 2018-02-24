#define _XOPEN_SOURCE 600
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <pthread.h>

#define SHM_NAME "shmnm"

#define SENSIBLE_VALUE 100
#define ARRAY_LENGTH 500
#define ARRAY_SIZE 500 * sizeof(int)

#define println(STR) printf(STR "\n");
#define print(FORMAT, args ...) printf(FORMAT "\n", args);

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

int create_shared_memory(int **mem, const char *name){
  int memory_id = shm_open(name, O_CREAT | O_RDWR, 0666);
  CHECK_NEGATIVE("NEW_SHARED_MEM", memory_id, "Cannot obtain shared memory");
  int res = ftruncate(memory_id, ARRAY_SIZE);
  CHECK_NEGATIVE("NEW_SHARED_MEM", res, "Cannot set memory size");
  void *sm = mmap(NULL, ARRAY_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, memory_id, 0);
  CHECK_NEGATIVE("NEW_SHARED_MEM", sm, "Cannot map memory");
  *mem = (int*) sm;
  CHECK_NEGATIVE("NEW_SHARED_MEM", *mem, "Cannot attach memory")
  return memory_id;
}

#define DELETE_SHARED_MEM(MEM, NAME) \
  CHECK_NEGATIVE("DELETE_SHARED_MEM", munmap(MEM, ARRAY_SIZE), "Cannot unmap memory");\
  CHECK_NEGATIVE("DELETE_SHARED_MEM", shm_unlink(NAME), "Cannot unlink memory");

void setup(char **argv);
void teardown();
void *reader(void *arg);
void *writer(void *arg);

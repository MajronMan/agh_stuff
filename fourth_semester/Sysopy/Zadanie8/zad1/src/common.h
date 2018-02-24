#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>

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

#define println(STR) printf(STR "\n");
#define print(FORMAT, args ...) printf(FORMAT "\n", args);

#define MAX_INT_LEN 10
#define RECORD_SIZE 1024
#define RECORD_TXT_SIZE 1014

typedef struct record {
  int id;
  char txt[RECORD_TXT_SIZE];
} record;

typedef struct thread_info {    /* Used as argument to thread_start() */
  pthread_t thread_id;          /* ID returned by pthread_create() */
  int thread_num;               /* Application-defined thread # */
  FILE *f;
} thread_info;

int text_length(int offset) {
  return RECORD_SIZE/sizeof(char) - offset;
}

char is_empty(char *line){
  for(int i=0; line[i]; i++) {
    if(!isspace(line[i]))
      return 0;
  }
  return 1;
}

int naive_search(char *txt,  char *pattern){
  int n = strlen(txt), m = strlen(pattern);
  for(int i=0; i<=n-m; i++) {
    char found = 1;
    for(int j=0; j<m; j++) {
      if(txt[i+j] != pattern[j]) {
        found = 0;
        break;
      }
    }
    if(found) return i;
  }
  return -1;
}

record parse_record(char *txt){
  char id_str[MAX_INT_LEN+1] = {};
  strncpy(id_str, txt, MAX_INT_LEN);
  record r;
  r.id = atoi(id_str);
  strcpy(r.txt, txt+MAX_INT_LEN);

  return r;
}


#endif /* end of include guard: COMMON_H */

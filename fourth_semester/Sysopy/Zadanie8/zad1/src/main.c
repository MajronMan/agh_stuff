#include "main.h"
#include "common.h"


void *(*seek[4])(void *);
int result;

int main(int argc, char** argv){
  srand((unsigned) time(NULL));
  CHECK_UNEQUAL("main", argc, 6, "Usage: ./main thread_count file_name records_count seeked_word option")

  seek[1] = seek1;
  seek[2] = seek2;
  seek[3] = seek3;

  thread_count = atoi(argv[1]);
  char *file_name = argv[2];
  records_count = atoi(argv[3]);
  seeked_word = argv[4];
  int option = atoi(argv[5]);

  fd = open(file_name, O_RDONLY);
  thread_ids = calloc(thread_count, sizeof(pthread_t));

  for(int i=0; i<thread_count; i++) {
    printf("Creating %d\n", i);

    pthread_create(thread_ids+i, NULL, *(seek + option), NULL);
  }
  for (int i = 0; i < thread_count; i++) {
    if(thread_ids[i] <= 0) {
      println("He's already dead")
      continue;
    }
    pthread_join(thread_ids[i], NULL);
    printf("%lu is done\n", thread_ids[i]);
  }
  print("CLOSING: IT'S IN %d", result)

  free(thread_ids);
  close(fd);
  return 0;
}

void *seek1(void *arg){
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  char acc[RECORD_SIZE + 1]={};
  while(1) {
    pthread_mutex_lock(&mtx);
    read(fd, acc, RECORD_SIZE * sizeof(char));
    records_count--;
    pthread_mutex_unlock(&mtx);


    if(records_count < 0) {
      break;
    }

    record r = parse_record(acc);
    char c = naive_search(r.txt, seeked_word);
    print("Thread %lu: read record %d and got %d while seeking", pthread_self(), r.id, c);
    if(c >= 0) {
      print("%lu FOUND IT: KILLIN EM ALL", pthread_self())
      for(int i=0; i<thread_count; i++) {
        if(thread_ids[i] == pthread_self()) {
          println("Not gonna kill myself")
          continue;
        }
        if(thread_ids[i] <= 0) {
          println("He's already dead")
          continue;
        }
        print("DIE DIE DIE MY DARLING %d %lu", i, thread_ids[i])
        pthread_cancel(thread_ids[i]);
        pthread_join(thread_ids[i], NULL);
        thread_ids[i] = 0;
      }
      result = r.id;
      println("JOB IS DONE")
      return (void *) 1;
    }
    wait(900);

  }
  return (void *) 0;
}

void *seek2(void *arg){
  pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
  char acc[RECORD_SIZE + 1]={};

  while(1) {
    print("%lu is still alive", pthread_self())
    pthread_mutex_lock(&mtx);
    read(fd, acc, RECORD_SIZE * sizeof(char));
    records_count--;
    pthread_mutex_unlock(&mtx);
    pthread_testcancel();
    print("%lu can die now", pthread_self())

    if(records_count < 0) {
      break;
    }

    record r = parse_record(acc);
    char c = naive_search(r.txt, seeked_word);
    print("Thread %lu: read record %d and got %d while seeking", pthread_self(), r.id, c);

    if(c >= 0) {
      print("%lu FOUND IT: KILLIN EM ALL", pthread_self())
      for(int i=0; i<thread_count; i++) {
        if(thread_ids[i] == pthread_self()) continue;
        print("DIE DIE DIE MY DARLING %d %lu", i, thread_ids[i])
        pthread_cancel(thread_ids[i]);
      }
      result = r.id;
      println("JOB IS DONE")
      return (void *) 1;
    }
    wait(500);
  }
  return (void *) 0;
}
void *seek3(void *arg){
  char acc[RECORD_SIZE + 1]={};

  while(1) {
    pthread_mutex_lock(&mtx);
    read(fd, acc, RECORD_SIZE * sizeof(char));
    records_count--;
    pthread_mutex_unlock(&mtx);


    if(records_count < 0) {
      break;
    }

    record r = parse_record(acc);
    char c = naive_search(r.txt, seeked_word);
    print("Thread %lu: read record %d and got %d while seeking", pthread_self(), r.id, c);
    if(c >= 0) {
      result = r.id;
      return (void *) 1;
    }
    wait(500)

  }
  return (void *) 0;
}

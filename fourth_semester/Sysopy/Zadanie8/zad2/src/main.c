#include "main.h"
#include "common.h"
#include <signal.h>


int result;
sigset_t sig_mask;

void handler(int sig){
  print("%d: %lu got %d", getpid(), pthread_self(), sig);
  exit(0);
}

void *seek_and_destroy(void *arg){

  for(int i=0; i<thread_count; i++) {
    print("IT'S TIME TO STOP %lu", thread_ids[i])
    pthread_kill(thread_ids[i], SIGTERM);
  }
  println("KILLED 'EM ALL")
  return (void*) 0;
}

int main(int argc, char** argv){
  srand((unsigned) time(NULL));
  CHECK_UNEQUAL("main", argc, 5, "Usage: ./main thread_count file_name records_count seeked_word")

  thread_count = atoi(argv[1]);
  char *file_name = argv[2];
  records_count = atoi(argv[3]);
  seeked_word = argv[4];

  signal(SIGUSR1, handler);
  signal(SIGTERM, handler);

  fd = open(file_name, O_RDONLY);
  thread_ids = calloc(thread_count+1, sizeof(pthread_t));

  for(int i=0; i<thread_count; i++) {
    printf("Creating %d\n", i);

    pthread_create(thread_ids+i, NULL, seek, NULL);
  }
  // sigfillset(&sig_mask);
  // sigprocmask(SIG_BLOCK, &sig_mask, NULL);
  // while(1) {
  //   print("MAIN (%lu) IS ALIVE", pthread_self())
  //   wait(1, 500)
  // }

  pthread_create(thread_ids+thread_count, NULL, seek_and_destroy, NULL);

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

void *seek(void *arg){
  signal(SIGUSR1, handler);
  signal(SIGTERM, handler);
  // sigset_t set;
  // sigfillset(&set);
  // sigprocmask(SIG_BLOCK, &set, NULL);

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
    print("Thread %lu of %d: read record %d and got %d while seeking", pthread_self(), getpid(), r.id, c);
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
    wait(1,0);

  }
  return (void *) 0;
}

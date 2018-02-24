#include "common.h"
#include "queue.h"

static char doexit = 0, wait_for_sigusr;
static sigset_t sig_mask, sig_old_mask;
sem_t *pillow_sem, *queue_sem, *seat_sem;
static int queue_mem_id;
static  queue *q;

void handler(int sig);
void wait_for_customer();

int main(int argc, char **argv) {
  CHECK_UNEQUAL("main", argc, 2, "Required number of seats")
  int chairs = atoi(argv[1]);

  CREATE_SEMAPHORE(pillow_sem, PILLOW_SEM_NAME)
  CREATE_SEMAPHORE(queue_sem, QUEUE_SEM_NAME)
  CREATE_SEMAPHORE(seat_sem, SEAT_SEM_NAME)
  NEW_SHARED_Q(q, queue_mem_id, SHARED_Q_NAME, chairs)

  while (!doexit) {
    prolaag(queue_sem);
    printf("Barber is gonna check da Q at %10.ld\n", get_timestamp());
    if (is_empty(q)) {
      prolaag(pillow_sem);
      printf("Barber is gonna sleep at %10.ld\n", get_timestamp());
      q->pillow = getpid();
      wait_for_customer();
      verhoog(pillow_sem);
      verhoog(queue_sem);

      while(wait_for_sigusr) {
        sigsuspend(&sig_old_mask);
      }
      sigprocmask(SIG_UNBLOCK, &sig_mask, NULL);
      prolaag(pillow_sem);
      printf("Barber has woken at %.10ld\n", get_timestamp());
      q->pillow = -1;
      verhoog(pillow_sem);

    } else {
      prolaag(seat_sem);
      pid_t customer = q->seat = pop(q);
      verhoog(queue_sem);
      printf("Barber is gonna cut sum %d at %10.ld\n", customer, get_timestamp());
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 150;
      select(0, NULL, NULL, NULL, &tv);
      printf("Cutting takes some time\n");
      CHECK_NEGATIVE("main", kill(q->seat, SIGUSR1), "Cannot send signal to customer")
      q->seat = -1;
      verhoog(seat_sem);
      printf("Barber is done with %d at %10.ld\n", customer, get_timestamp());
    }
  }

  munmap((void *) q, sizeof(queue));
  shm_unlink(SHARED_Q_NAME);
  sem_close(pillow_sem);
  sem_unlink(PILLOW_SEM_NAME);
  sem_close(queue_sem);
  sem_unlink(QUEUE_SEM_NAME);
  sem_close(seat_sem);
  sem_unlink(SEAT_SEM_NAME);
}

void handler(int sig) {
  if (sig == SIGINT) {
    printf("Barber cannot stand interruption!\n");
    doexit = 1;
    wait_for_sigusr = 0;
  }
  if (sig == SIGUSR1) {
    wait_for_sigusr = 0;
  }
}

void wait_for_customer() {
  CHECK_NEGATIVE("wait_for_customer", signal(SIGUSR1, handler), "Cannot create SIGUSR1 handler")
  CHECK_NEGATIVE("wait_for_customer", signal(SIGINT, handler), "Cannot create SIGINT handler")
  sigfillset(&sig_mask);
  sigdelset(&sig_mask, SIGUSR1);
  sigdelset(&sig_mask, SIGINT);
  sigprocmask(SIG_BLOCK, &sig_mask, &sig_old_mask);
  wait_for_sigusr = 1;
}

void free_res(){
  munmap((void *) q, sizeof(queue));
  shm_unlink(SHARED_Q_NAME);
  sem_close(pillow_sem);
  sem_unlink(PILLOW_SEM_NAME);
  sem_close(queue_sem);
  sem_unlink(QUEUE_SEM_NAME);
  sem_close(seat_sem);
  sem_unlink(SEAT_SEM_NAME);
}
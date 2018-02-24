#include "common.h"
#include "queue.h"

static char doexit = 0, wait_for_sigusr;
static sigset_t sig_mask, sig_old_mask;
int pillow_sem_id, queue_sem_id, seat_sem_id, queue_mem_id;
void handler(int sig);
void wait_for_customer();

int main(int argc, char **argv) {
  CHECK_UNEQUAL("main", argc, 2, "Required number of seats")
  int chairs = atoi(argv[1]);

  sembuf semaphore;
  queue *q;

  CREATE_SEMAPHORE(pillow_sem_id, PILLOW_SEM_KEYGEN)
  CREATE_SEMAPHORE(queue_sem_id, QUEUE_SEM_KEYGEN)
  CREATE_SEMAPHORE(seat_sem_id, SEAT_SEM_KEYGEN)
  NEW_SHARED_Q(q, queue_mem_id, QUEUE_MEM_KEYGEN, chairs)

  while (!doexit) {
    prolaag(queue_sem_id, &semaphore);
    printf("Barber is gonna check da Q at %10.ld\n", get_timestamp());
    if (is_empty(q)) {
      prolaag(pillow_sem_id, &semaphore);
      printf("Barber is gonna sleep at %10.ld\n", get_timestamp());
      q->pillow = getpid();
      wait_for_customer();
      verhoog(pillow_sem_id, &semaphore);
      verhoog(queue_sem_id, &semaphore);

      while(wait_for_sigusr) {
        sigsuspend(&sig_old_mask);
      }
      sigprocmask(SIG_UNBLOCK, &sig_mask, NULL);
      prolaag(pillow_sem_id, &semaphore);
      printf("Barber has woken at %.10ld\n", get_timestamp());
      q->pillow = -1;
      verhoog(pillow_sem_id, &semaphore);

    } else {
      prolaag(seat_sem_id, &semaphore);
      pid_t customer = q->seat = pop(q);
      verhoog(queue_sem_id, &semaphore);
      printf("Barber is gonna cut sum %d at %10.ld\n", customer, get_timestamp());
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 150;
      select(0, NULL, NULL, NULL, &tv);
      printf("Cutting takes some time\n");
      CHECK_NEGATIVE("main", kill(q->seat, SIGUSR1), "Cannot send signal to customer")
      q->seat = -1;
      verhoog(seat_sem_id, &semaphore);
      printf("Barber is done with %d at %10.ld\n", customer, get_timestamp());
    }
  }

  shmdt(q);
  semctl(pillow_sem_id, 0, IPC_RMID, 0);
  semctl(queue_sem_id, 0, IPC_RMID, 0);
  semctl(seat_sem_id, 0, IPC_RMID, 0);
  shmctl(queue_mem_id, IPC_RMID, NULL);
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

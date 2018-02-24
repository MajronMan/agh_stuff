#include "customers.h"

int main(int argc, char **argv) {
  CHECK_UNEQUAL("main", argc, 3,
                "Usage: ./customers customers_count haircuts_count")

  customers_count = (unsigned int) atoi(argv[1]);
  haircuts_count = (unsigned int) atoi(argv[2]);
  pid_t *pids = calloc(customers_count, sizeof(pid_t));


  GET_SEMAPHORE(queue_sem_id, QUEUE_SEM_KEYGEN)
  GET_SEMAPHORE(seat_sem_id, SEAT_SEM_KEYGEN)
  GET_SEMAPHORE(pillow_sem_id, PILLOW_SEM_KEYGEN)
  GET_SHARED_Q(q, queue_mem_id, QUEUE_MEM_KEYGEN)

  for (int i = 0; i < customers_count; i++) {
    CHECK_NEGATIVE("create_customer", (pids[i] = fork()),
                   "Cannot create new customer process")

    if (pids[i] == 0) {
      do_customer_stuff();
      printf("%d is free!\n", getpid());
      exit(0);
    }
  }

  wait(NULL);

  free(pids);
  shmdt(q);
  return 0;
}

void do_customer_stuff() {
  pid_t my_pid = getpid();
  my_haircuts = 0;
  printf("%d is gonna do some customer stuff at %10.ld\n", my_pid, get_timestamp());

  while (my_haircuts < haircuts_count) {
    printf("%d is gonna get a haircut at %10.ld\n", my_pid, get_timestamp());
    if (in_da_shop()) {
      wait_for_barber();
      while(wait_for_sigusr) {
        sigsuspend(&sig_old_mask);
      }
      sigprocmask(SIG_UNBLOCK, &sig_mask, NULL);
      printf("%d is leaving the shop with awesome haircut at %10.ld\n", my_pid, get_timestamp());
    } else {
      printf("%d is leaving the shop due to lack of space at %10.ld\n", my_pid, get_timestamp());
      my_haircuts++;
    }
  }
}

char in_da_shop() {
  sembuf semaphore;
  pid_t my_pid = getpid();
  char result;

  prolaag(queue_sem_id, &semaphore);
  if (!is_empty(q)) {
    printf("%d sees there are people waiting and joins queue at %.10ld\n", my_pid, get_timestamp());
    result = push(q, my_pid);
    verhoog(queue_sem_id, &semaphore);
  } else {
    prolaag(seat_sem_id, &semaphore);
    if (q->seat > 0) {
      printf("%d sees seat is taken and joins queue at %.10ld\n",
             my_pid, get_timestamp());
      result = push(q, my_pid);
      verhoog(queue_sem_id, &semaphore);
      verhoog(seat_sem_id, &semaphore);
    } else {
      prolaag(pillow_sem_id, &semaphore);
      if (q->pillow <= 0) {
        printf("%d sees barber is not asleep and joins queue at %.10ld\n",
               my_pid, get_timestamp());
        result = push(q, my_pid);
        verhoog(queue_sem_id, &semaphore);
        verhoog(seat_sem_id, &semaphore);
        verhoog(pillow_sem_id, &semaphore);
      } else {
        printf("%d sees barber %d is sleeping and wakes him at %.10ld\n",
               my_pid, q->pillow, get_timestamp());
        push(q, my_pid);
        kill(q->pillow, SIGUSR1);

        verhoog(queue_sem_id, &semaphore);
        verhoog(seat_sem_id, &semaphore);
        verhoog(pillow_sem_id, &semaphore);
        result = 1;
      }
    }
  }
  return result;
}

void wait_for_barber() {
  CHECK_NEGATIVE("wait_for_barber", signal(SIGUSR1, handler), "Cannot create SIGUSR1 handler")
  sigfillset(&sig_mask);
  sigdelset(&sig_mask, SIGUSR1);
  sigprocmask(SIG_BLOCK, &sig_mask, &sig_old_mask);
  wait_for_sigusr = 1;
}

void handler(int sig) {
  if (sig == SIGUSR1) {
    my_haircuts++;
    wait_for_sigusr = 0;
  }
}

#ifndef CUSTOMERS_H
#define CUSTOMERS_H

#include "common.h"
#include "queue.h"
static int queue_sem_id, seat_sem_id, pillow_sem_id, queue_mem_id;
static unsigned int customers_count, haircuts_count;
static queue *q;
static sigset_t sig_mask, sig_old_mask;
static int my_haircuts;
static char wait_for_sigusr;

void do_customer_stuff();
char in_da_shop();
void wait_for_barber();
void handler(int sig);

#endif

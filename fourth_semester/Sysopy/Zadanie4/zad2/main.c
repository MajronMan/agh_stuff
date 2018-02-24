#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#include <sys/time.h>
#include <wait.h>

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    char msg[100];\
    sprintf(msg, "%s: %s\n", FUN, ERR);\
    perror(msg);\
    exit(-1);\
}

#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

#define TIMEVALDELTA(T1, T2) T2.tv_sec + T2.tv_usec/1e6 - T1.tv_sec - T1.tv_usec/1e6

#define HELP printf("Usage: ./main.out children_count mercy_count\n");
#define MAX_TIME 10

#define RED     "\x1b[31m"
#define GREEN   "\x1b[32m"
#define MAGENTA "\x1b[35m"
#define CYAN    "\x1b[36m"
#define GREY    "\x1b[90m"
#define LBLUE    "\x1b[94m"
#define LGREY   "\x1b[38;5;244m"
#define RESET   "\x1b[0m"

typedef struct timespec timespec;
typedef struct timeval timeval;

void do_child_stuff(pid_t parent_pid, int i);

void create_children(pid_t parent_pid);

void SIGUSR1_handler(int sig, siginfo_t *siginfo, void *context);

void SIGINT_handler(int sig, siginfo_t *siginfo, void *context);

void SIGRT_handler(int sig, siginfo_t *siginfo, void *context);

size_t N, M;
pid_t *received;
pid_t *pids;
int received_count = 0;
int i = 0;
char interrupt = 0;

int main(int argc, char **argv) {
    if (argc < 3) {
        HELP
        exit(-1);
    }
    N = (size_t) atoi(argv[1]);
    M = (size_t) atoi(argv[2]);

    received = calloc(sizeof(pid_t *), N + 1);
    pids = calloc(sizeof(pid_t *), N + 1);

    create_children(getpid());

    struct sigaction act;
    memset(&act, '\0', sizeof(act));
    act.sa_sigaction = &SIGUSR1_handler;
    act.sa_flags = SA_SIGINFO;
    CHECK_NEGATIVE_ONE("main", sigaction(SIGUSR1, &act, NULL), "SIGUSR1 install error")

    struct sigaction act2;
    memset(&act2, '\0', sizeof(act2));
    act2.sa_sigaction = &SIGINT_handler;
    act2.sa_flags = SA_SIGINFO;
    CHECK_NEGATIVE_ONE("main", sigaction(SIGINT, &act2, NULL), "SIGINT install error")

    struct sigaction act3;
    memset(&act3, '\0', sizeof(act3));
    act3.sa_sigaction = &SIGRT_handler;
    act3.sa_flags = SA_SIGINFO;
    for (int i = 0; i < 16; i++) {
        CHECK_NEGATIVE_ONE("main", sigaction(SIGRTMIN + i, &act3, NULL), "SIGRT install error")
        CHECK_NEGATIVE_ONE("main", sigaction(SIGRTMAX - i, &act3, NULL), "SIGRT install error")
    }

    while (received_count < N) {
        sleep(2 * MAX_TIME);
    }
    sleep(2);

    free(received);
    free(pids);
    return 0;
}


void create_children(pid_t parent_pid) {
    for (int i = 0; i < N; ++i) {
        CHECK_NEGATIVE_ONE("create_children", pids[i] = fork(), "Cannot create child")
        if (pids[i] == 0) {
            srand((unsigned int) (time(NULL) ^ (getpid() << 16)));
            do_child_stuff(parent_pid, i);
            exit(0);
        }
    }
}

int get_random_rt_signal() {
    if (rand() % 2)
        return SIGRTMIN + (rand() % 16);
    else
        return SIGRTMAX - (rand() % 16);
}

void do_child_stuff(pid_t parent_pid, int i) {
    long long delta = (long long int) (MAX_TIME * 1e9 / (N + 1));
    long long nd = (long long int) ((i + 1) * (delta + (rand() % (delta / N))));
    long long sec = (long long int) (nd / 1e9);
    long long nsec = nd % ((long long int) 1e9);
    timespec t = {sec, nsec};
    timespec rem = {0, 0};
    nanosleep(&t, &rem);
    printf(GREY"Hi, my name is %d aka %d and I worked hard for %.4fs"RESET" \n", getpid(), i,
           t.tv_sec + (t.tv_nsec / 1e9));

    union sigval val = {.sival_int=i};
    sigqueue(parent_pid, SIGUSR1, val);
    timeval start_time;
    gettimeofday(&start_time, NULL);

    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGUSR2);
    sigprocmask(SIG_BLOCK, &set, NULL);
    int received_sig;
    CHECK_NEGATIVE_ONE("do_child_stuff", sigwait(&set, &received_sig), "waiting for signal failed")

    int signal = get_random_rt_signal();
    sigqueue(parent_pid, signal, val);
    timeval end_time;
    gettimeofday(&end_time, NULL);

    double dt = TIMEVALDELTA(start_time, end_time);
    printf(LGREY"%d is free after %lf!"RESET"\n", getpid(), dt);
    int exitcode = (int) dt;
    exit(exitcode);
}

void release_the_signal() {
    for (; i < received_count; i++) {
        printf(GREEN"Allowed %d to continue"RESET"\n", received[i]);
        union sigval val = {.sival_int=1, .sival_ptr=""};
        sigqueue(received[i], SIGUSR2, val);
    }
}

void SIGUSR1_handler(int sig, siginfo_t *siginfo, void *context) {
    printf(LBLUE"This is parent, received SIGUSR1 from %d"RESET"\n", siginfo->si_pid);

    received[received_count++] = siginfo->si_pid;
    if (received_count >= M) {
        release_the_signal();
    }
}

void SIGINT_handler(int sig, siginfo_t *siginfo, void *context) {
    if (interrupt) return;
    interrupt = 1;
    printf(RED"Got interrupted\n");
    for (int i = 0; i < N; i++) {
        if (pids[i]) {
            printf(RED"Killing %d"RESET"\n", pids[i]);
            kill(pids[i], SIGKILL);
            pids[i] = 0;
        }
    }
    free(pids);
    free(received);
    exit(1);
}

void SIGRT_handler(int sig, siginfo_t *siginfo, void *context) {
    printf(CYAN"This is parent, received rt signal %d from %d"RESET"\n", sig, siginfo->si_pid);
    int wstatus;
    pids[siginfo->si_value.sival_int] = 0;
    waitpid(siginfo->si_pid, &wstatus, 0);
    if (WIFEXITED(wstatus)) {
        printf(MAGENTA"%d exited with code %d"RESET"\n", siginfo->si_pid, WEXITSTATUS(wstatus));
    }
}

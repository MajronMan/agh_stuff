#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    char msg[100];\
    sprintf(msg, "%s: %s\n", FUN, ERR);\
    perror(msg);\
    exit(-1);\
}

#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)
#define CHECK_SIG_ERR(FUN, VAL, ERR) CHECK(FUN, VAL, SIG_ERR, ERR)

#define HELP printf("Usage: ./main.out signals_count Type\n");

#define RED     "\x1b[31m"
#define GREEN   "\x1b[32m"
#define BLUE    "\x1b[34m"
#define MAGENTA "\x1b[35m"
#define CYAN    "\x1b[36m"
#define RESET   "\x1b[0m"

typedef struct timespec timespec;
typedef struct timeval timeval;

void do_child_stuff();

void do_parent_stuff();

int send_signal(pid_t pid, int sig);

int select_rt_signal(int sig);

void prepare_set(sigset_t *set);

void SIGINT_handler(int sig);

int L;
int Type;
pid_t child_pid;
pid_t parent_pid;

int main(int argc, char **argv) {
    if (argc != 3) {
        HELP
        exit(-1);
    }
    L = atoi(argv[1]);
    Type = atoi(argv[2]);
    parent_pid = getpid();

    CHECK_SIG_ERR("main", signal(SIGINT, SIGINT_handler), "SIGINT install error");

    CHECK_NEGATIVE_ONE("main", child_pid = fork(), "Failed to fork")
    if (child_pid == 0) {
        do_child_stuff();
    } else {
        do_parent_stuff();
    }
    return 0;
}

char *get_child_msg(){
    char *what;
    switch(Type){
        case 1:
            what = "kill";
            break;
        case 2:
            what = "add some sigqueue to";
            break;
        case 3:
            what = "do some realtime to";
            break;
        default:
            what = "WHAT";
            break;
    }
    return what;
}

void do_child_stuff() {
    int received = 0;
    int received_sig = 0;
    while(received_sig != select_rt_signal(SIGUSR2)) {
        sigset_t set;
        prepare_set(&set);
        CHECK_NEGATIVE_ONE("do_child_stuff", sigwait(&set, &received_sig), "waiting for signal failed")
        printf(GREEN"CHILD RECEIVED SIGNAL %d"RESET"\n", received_sig);
        received += received_sig == select_rt_signal(SIGUSR1);
    }
    printf(MAGENTA"Time has come to %s the parent %d times"RESET"\n", get_child_msg(), received);
    for(int i=0; i<received; i++) {
        send_signal(parent_pid, SIGUSR1);
        sleep(1);
    }
}

void do_parent_stuff() {
    sleep(1);
    int sent = 0, received = 0;
    for(int i=0; i<L; i++) {
        sent++;
        send_signal(child_pid, SIGUSR1);
        sleep(1);
    }
    char *sig1 = Type == 3? "SIGRTMIN": "SIGUSR1";
    char *sig2 = Type == 3? "SIGRTMAX": "SIGUSR2";
    printf(BLUE"Parent sent %d %s, time for some juicy %s"RESET"\n", sent, sig1, sig2);
    send_signal(child_pid, SIGUSR2);

    while(received < L) {
        int received_sig;
        sigset_t set;
        prepare_set(&set);
        CHECK_NEGATIVE_ONE("do_parent_stuff", sigwait(&set, &received_sig), "waiting for signal failed")
        printf(GREEN"PARENT RECEIVED SIGNAL %d"RESET"\n", received_sig);
        received++;
    }
    sleep(1);
    printf(CYAN"Parent received %d signals"RESET"\n", received);

}

int select_rt_signal(int sig){
    if(Type != 3) return sig;
    if(sig == SIGUSR1) return SIGRTMIN;
    else return SIGRTMAX;
}

int send_signal(pid_t pid, int sig){
    union sigval s={0};
    if(Type == 2) return sigqueue(pid, sig, s);
    return kill(pid, select_rt_signal(sig));
}

void prepare_set(sigset_t *set) {
    sigemptyset(set);
    sigaddset(set, SIGUSR2);
    sigaddset(set, SIGUSR1);
    sigaddset(set, SIGRTMIN);
    sigaddset(set, SIGRTMAX);
    sigprocmask(SIG_BLOCK, set, NULL);
}

void SIGINT_handler(int sig) {
    printf(RED"INTERRUPTED BY %d"RESET"\n", sig);
    kill(child_pid, SIGKILL);
    exit(1);
}
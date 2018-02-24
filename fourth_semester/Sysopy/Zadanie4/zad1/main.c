#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}
#define CHECK_SIG_ERR(FUN, VAL, ERR) CHECK(FUN, VAL, SIG_ERR, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

void SIGINT_handler(int);

static void SIGTSTP_handler(int sig, siginfo_t *siginfo, void *context);

char dc = 1;

int main(int argc, char *argv[]) {
    struct sigaction act;
    memset(&act, '\0', sizeof(act));
    act.sa_sigaction = &SIGTSTP_handler;
    act.sa_flags = SA_SIGINFO;

    CHECK_SIG_ERR("main", signal(SIGINT, SIGINT_handler), "SIGINT install error")
    CHECK_NEGATIVE_ONE("main", sigaction(SIGTSTP, &act, NULL), "SIGTSTP install error")

    char delta = 'Z' - 'A' + 1;
    for (char c = 0;; c = (c + dc + delta) % delta) {
        printf("%c\n", 'A' + c);
        sleep(1);
    }
}

void SIGINT_handler(int sig) {
    printf("Odebrano sygnał SIGINT\n");
    exit(0);
}

static void SIGTSTP_handler(int sig, siginfo_t *siginfo, void *context) {
    dc *= -1;
}

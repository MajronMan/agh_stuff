#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wait.h>
#include <string.h>
#include <ctype.h>
#include <sys/resource.h>

#define MAX_ARGS 16

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

#define CHECK_NON_ZERO(FUN, VAL, ERR)if((VAL) != 0) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

#define CHECK_ZERO(FUN, VAL, ERR) CHECK(FUN, VAL, 0, ERR)
#define CHECK_NULL(FUN, VAL, ERR) CHECK(FUN, VAL, NULL, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

#define HELP printf("Usage: ./main path/to/interpreted/file \n");

void help();
char interpret_line(FILE *file, int num);
char is_empty(char *line);
void parse(char *line, int num);
void exec(char **argv, int num);


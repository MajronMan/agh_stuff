#ifndef ZADANIE5_MAIN_H
#define ZADANIE5_MAIN_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wait.h>
#include <string.h>
#include <ctype.h>
#include <sys/resource.h>

#define MAX_ARGS 16

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    char msg[100];\
    sprintf(msg, "%s: %s\n", FUN, ERR);\
    perror(msg);\
    exit(-1);\
}

#define CHECK_NON_ZERO(FUN, VAL, ERR)if((VAL) != 0) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

#define CHECK_ZERO(FUN, VAL, ERR) CHECK(FUN, VAL, 0, ERR)
#define CHECK_NULL(FUN, VAL, ERR) CHECK(FUN, VAL, NULL, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

#define CHECK_ARGS(VAL) if(argc != VAL){\
    printf("Usage: ./main path/to/interpreted/file \n");\
    exit(-1);\
}

#define PIPE "|"
#define DELIMS " \n\t\r"
#define READ 0
#define WRITE 1
#define BUFFER_SIZE 100000

struct line_data{
    char *argv[20][5];
    int argc[20];
    int command_count;
};

typedef struct line_data line_data;

char read_line(FILE *file, int num);
char is_empty(char *line);
void parse(char *line, int num);
void stackstuff(line_data *data);
void create_children(line_data *data);
void exec(char **argv, int num);

#endif //ZADANIE5_MAIN_H

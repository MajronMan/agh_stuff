#ifndef ZADANIE6_COMMON_H
#define ZADANIE6_COMMON_H

#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <ctype.h>
#include <time.h>

#define PROJECT_ID 128
#define MAX_CONTENT_SIZE 1024
#define MSG_SIZE sizeof(msg) - sizeof(long)
#define GET_KEY ftok("/home", PROJECT_ID)

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

#define NEW_MSG3C(MSG, TYPE, CONTENT) MSG.type = TYPE;\
strcpy(MSG.contents, CONTENT);
#define NEW_MSG3I(MSG, TYPE, INT) MSG.type = TYPE;\
MSG.int_content = INT;

#define NEW_MSG4(MSG, TYPE, CONTENT, INT) \
MSG.type = TYPE;\
strcpy(MSG.contents, CONTENT);\
MSG.int_content = INT;

#define SEND(QID, MSG) msgsnd(QID, MSG, MSG_SIZE, 0)
#define RECEIVE(QID, MSG) msgrcv(QID, MSG, MSG_SIZE, 0, 0)

typedef struct msg {
    long type;
    char contents[MAX_CONTENT_SIZE];
    unsigned int_content;
} msg;

typedef enum msg_types {
    HELLO = 12,
    BYE,
    OK,
    NOPE,
    ECHO,
    UPPER,
    TIME,
    DIE
} msg_types;
#endif //ZADANIE6_COMMON_H

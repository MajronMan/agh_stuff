#ifndef ZADANIE6_COMMON_H
#define ZADANIE6_COMMON_H

#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <mqueue.h>

#define PROJECT_ID 128
#define MAX_CONTENT_SIZE 1024
#define MSG_SIZE sizeof(msg)
#define MSG_LMT 10
#define SERVER_NAME "/server"

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

#define SEND(QID, MSG) mq_send(QID, (char *)MSG, MSG_SIZE, 0)
#define RECEIVE(QID, MSG) mq_receive(QID, (char *) MSG, MSG_SIZE, 0)


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

mqd_t open_queue(const char *name, char mode) {
    struct mq_attr attr = {0, MSG_LMT, MSG_SIZE};

    mqd_t q;
    if (mode == 'r') {
        CHECK_NEGATIVE_ONE("open_queue", q =  mq_open(name, O_CREAT | O_RDONLY, 0666, &attr),
                           "Cannot open queue for read")
    } else {
        CHECK_NEGATIVE_ONE("open_queue", q = mq_open(name, O_WRONLY), "Cannot open queue for write")
    }
    return q;

}

char not_empty(mqd_t qid){
    struct mq_attr attr;
    CHECK_NEGATIVE_ONE("not_empty", mq_getattr(qid, &attr), "Cannot get queue's attrs")
    return attr.mq_curmsgs != 0;
}
#endif //ZADANIE6_COMMON_H
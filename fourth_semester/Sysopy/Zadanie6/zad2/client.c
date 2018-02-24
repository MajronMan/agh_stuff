#include "client.h"

int main(int argc, char **argv) {
    srand(time(NULL));

    gen_random_name(q_name);
    CHECK_NEGATIVE_ONE("main", my_qid = open_queue(q_name, 'r'), "Cannot create private queue")
    CHECK_NEGATIVE_ONE("main", server_qid = open_queue(SERVER_NAME, 'w'), "Cannot open server queue")

    char cmd[6];
    id = sayHello();

    while (!doexit) {
        printf("enter ECHO|UPPER|TIME|BYE|DIE $ ");
        scanf("%s", cmd);

        if (handle_command(cmd)) {
            msg message;
            RECEIVE(my_qid, &message);
            if (message.type == DIE) {
                printf("Server is dead\n");
                break;
            }

            printf("Server responded: %s\n", message.contents);
        }
    }

    CHECK_NEGATIVE_ONE("main", mq_close(server_qid), "Cannot close server queue")
    CHECK_NEGATIVE_ONE("main", mq_close(my_qid), "Cannot close client queue")
    CHECK_NEGATIVE_ONE("main", mq_unlink(q_name), "Cannot delete client queue")
    return 0;
}

char handle_command(char *cmd) {
    char text[MAX_CONTENT_SIZE] = {};
    msg message;
    char c = tolower(cmd[0]);
    char scan = 0;
    char ret = 1;
    long type;
    switch (c) {
        case 'e':
            type = ECHO;
            scan = 1;
            break;
        case 'u':
            type = UPPER;
            scan = 1;
            break;
        case 't':
            type = TIME;
            break;
        case 'b':
            type = BYE;
            doexit = 1;
            ret = 0;
            break;
        case 'd':
            type = DIE;
            doexit = 1;
            break;
        default:
            printf("No such command\n");
            return 0;
    }
    if (scan) {
        printf("Enter text to send: ");
        scanf("%s", text);
    }
    NEW_MSG4(message, type, text, id);
    SEND(server_qid, &message);
    return ret;
}

int sayHello() {
    msg message = {};
    NEW_MSG3C(message, HELLO, q_name);

    SEND(server_qid, &message);

    RECEIVE(my_qid, &message);

    if (message.type == OK) {
        printf("Connected to server with ID %d\n", message.int_content);
        return message.int_content;
    }
    printf("Connection refused as: %s\n", message.contents);
    doexit = 1;
    return -1;
}

char *gen_random_name(char *s) {
    int len = NAME_LEN + 1;
    static const char alphanum[] =
            "0123456789"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "abcdefghijklmnopqrstuvwxyz";

    for (int i = 1; i < len; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }
    s[0] = '/';
    s[len-1] = 0;
    return s;
}
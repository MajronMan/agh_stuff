#include "client.h"

int main(int argc, char**argv){
    CHECK_NEGATIVE_ONE("main", key = GET_KEY, "Cannot acquire key");
    CHECK_NEGATIVE_ONE("main", my_qid = msgget(IPC_PRIVATE, 0666), "Cannot create private queue")
    CHECK_NEGATIVE_ONE("main", server_qid = msgget(key, 0), "Cannot open server queue")

    char cmd[6];
    id = sayHello();

    while(!doexit){
        printf("enter ECHO|UPPER|TIME|BYE|DIE $ ");
        scanf("%s", cmd);

        if(handle_command(cmd)){
            msg message;
            RECEIVE(my_qid, &message);
            if(message.type == DIE) {
                printf("Server is dead\n");
                break;
            }

            printf("Server responded: %s\n", message.contents);
        }
    }

    CHECK_NEGATIVE_ONE("main", msgctl(my_qid, IPC_RMID, NULL), "Cannot delere queue")
    return 0;
}

char handle_command(char *cmd) {
    char text[MAX_CONTENT_SIZE] = {};
    msg message;
    char c = tolower(cmd[0]);
    char scan = 0;
    char ret = 1;
    long type;
    switch(c){
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
    if(scan) {
        printf("Enter text to send: ");
        scanf("%s", text);
    }
    NEW_MSG4(message, type, text, id);
    SEND(server_qid, &message);
    return ret;
}

int sayHello(){
    msg message;
    NEW_MSG3I(message, HELLO, my_qid);

    SEND(server_qid, &message);

    RECEIVE(my_qid, &message);

    if(message.type == OK){
        printf("Connected to server with ID %d\n", message.int_content);
        return message.int_content;
    }
    printf("Connection refused as: %s\n", message.contents);
    doexit = 1;
    return -1;
}
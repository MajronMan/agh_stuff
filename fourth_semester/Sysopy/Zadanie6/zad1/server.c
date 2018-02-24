#include "server.h"

int main() {
    atexit(remove_queue);
    key_t key;
    msg message;

    CHECK_NEGATIVE_ONE("main", key = GET_KEY, "Cannot acquire key");
    CHECK_NEGATIVE_ONE("main", qid = msgget(key, IPC_CREAT | 0666), "Cannot create queue")

    printf("----------------\n|SERVER IS UP!!|\n----------------\n");
    while (!doexit) {
        CHECK_NEGATIVE_ONE("main", RECEIVE(qid, &message), "Failed to receive message")

        handle(&message);
    }
    printf("----------------\n|SHUTTING DOWN!|\n----------------\n");
    return 0;
}

void handle(msg *message) {
    CHECK_NULL("handle", message, "Message cannot be null")
    switch (message->type) {
        case HELLO:
            hello(message->int_content);
            break;
        case BYE:
            bye(message->int_content);
            break;
        case ECHO:
            echo(message->int_content, message->contents);
            break;
        case UPPER:
            upper(message->int_content, message->contents);
            break;
        case TIME:
            what_time_is_it(message->int_content);
            break;
        case DIE:
            printf("Time to die\n");
            doexit = 1;
            break;
        default:
            printf("No such command\n");
            break;
    }
}

void hello(int qid) {
    msg message;
    int id = get_next_client_id();
    if (id == -1) {
        printf("Server is full\n");
        NEW_MSG3C(message, NOPE, "Server is full")
    } else {
        printf("New client: %d\n", id);
        NEW_MSG3I(message, OK, id)
        clients[id] = qid;
        client_count++;
    }
    CHECK_NEGATIVE_ONE("hello", SEND(qid, &message), "Cannot send message")
}

void bye(int id) {
    printf("bye, bye %d!\n", id);
    client_count--;
    clients[id] = 0;
}

void echo(int id, char *what) {
    printf("%d wants me to repeat %s\n", id, what);
    msg message;
    NEW_MSG3C(message, ECHO, what)

    int qid = clients[id];
    CHECK_NEGATIVE_ONE("echo", SEND(qid, &message), "Cannot send message")
}

void upper(int id, char *what) {
    printf("%d wants some upper %s\n", id, what);
    int qid = clients[id];
    int l = (int) strlen(what);
    msg message;
    char content[MAX_CONTENT_SIZE] = {};

    for (int i = 0; i < l; i++) {
        content[i] = (char) toupper(what[i]);
    }
    NEW_MSG3C(message, UPPER, content)
    SEND(qid, &message);
}

void what_time_is_it(int id) {
    printf("%d wants to know the time\n", id);
    int qid = clients[id];
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    char dt[200];

    sprintf(dt, "now: %d-%d-%d %d:%d:%d\n",
            tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
            tm.tm_hour, tm.tm_min, tm.tm_sec);
    msg message;
    NEW_MSG3C(message, TIME, dt)
    SEND(qid, &message);
}

int get_next_client_id() {
    if (client_count >= MAX_CLIENTS) return -1;
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i] == 0) return i;
    }
    return -1;
}

void remove_queue() {
    CHECK_NEGATIVE_ONE("main", msgctl(qid, IPC_RMID, NULL), "Cannot delete queue")
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (clients[i]) SEND(clients[i], &shutdownMessage);
    }
}

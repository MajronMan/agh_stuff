#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>

#define CHECK_NULL(FUN, VAL, ERR) if((VAL) == NULL) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) if((VAL) == -1) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

struct node {
    struct flock lock;
    struct node *next;
};

void print_lock(struct flock lock) {
    char *locks_to_strings[10];
    locks_to_strings[F_EXLCK] = "exclusive";
    locks_to_strings[F_RDLCK] = "read";
    locks_to_strings[F_WRLCK] = "write";

    char *lock_string;
    if (lock.l_type < 0 || lock.l_type > 4) {
        lock_string = "unknown";
    } else {
        lock_string = locks_to_strings[lock.l_type];
    }
    printf("pid: %u, %s lock at %u\n", lock.l_pid, lock_string, (unsigned) lock.l_start);
}

void clear_list(struct node *head) {
    if (head) {
        clear_list(head->next);
        free(head);
    }
}

struct node *create_node(struct flock lock) {
    struct node *ret = malloc(sizeof(struct node));
    ret->lock = lock;
    ret->next = NULL;
    return ret;
}

struct node *create_empty() {
    struct node *ret = malloc(sizeof(struct node));
    struct flock lock;
    ret->lock = lock;
    ret->next = NULL;
    return ret;
}

struct node *push(struct node *head, struct flock lock) {
    struct node *new = create_node(lock);
    new->next = head->next;
    head->next = new;
    return head;
}

char compare(struct flock l1, struct flock l2) {
    return l1.l_type == l2.l_type &&
           l1.l_start == l2.l_start;
}

struct node *find(struct node *head, struct flock lock) {
    if (head->next) {
        if (compare(lock, head->next->lock)) {
            return head;
        }
        return find(head->next, lock);
    } else return NULL;
}

struct node *find_by_pos(struct node *head, int byte) {
    if (head->next) {
        if (head->next->lock.l_start == byte) {
            return head;
        }
        return find_by_pos(head->next, byte);
    } else return NULL;
}


char remove_lock(struct node *head, struct flock lock) {
    struct node *found = find(head, lock);
    if (!found) return 0;
    struct node *killme = found->next;
    found->next = found->next->next;
    free(killme);
    return 1;
}

void remove_lock_pos(struct node *head, int byte) {
    struct node *found = find_by_pos(head, byte);
    if (!found) return;
    struct node *killme = found->next;
    found->next = found->next->next;
    free(killme);
}

void print_list(struct node *head) {
    if (head) {
        print_list(head->next);
        print_lock(head->lock);
    }
}

struct node *locks_list;

void help() {
    printf("Type a command and accept with enter. If applicable, you will be prompted for options.\n");
    printf("Commands:\n "
                   "rl - set read lock without wait \n"
                   " rlw - set read lock with wait \n "
                   "wl - set write lock without wait\n "
                   "wlw - set write lock with wait \n "
                   "ls - list locks \n "
                   "drl - disable read lock \n "
                   "dwl - disable write lock \n "
                   "r - read byte \n "
                   "w - write byte \n "
                   "exit \n");
}

struct flock set_file_lock(int actionFlag, short lockType, int sys_file, off_t offset) {
    struct flock flock;
    flock.l_type = lockType;
    flock.l_whence = SEEK_SET;
    flock.l_start = offset;
    flock.l_len = 1;

    CHECK_NEGATIVE_ONE("set_file_lock", fcntl(sys_file, actionFlag, &flock), "Cannot obtain file lock")
    return flock;
}

void ls(int sys_file) {
    off_t file_size;
    CHECK_NEGATIVE_ONE("ls", file_size = lseek(sys_file, 0, SEEK_END), "Cannot determine file size")

    struct flock file_lock;
    for (off_t i = 0; i < file_size; ++i) {
        file_lock = set_file_lock(F_GETLK, F_WRLCK, sys_file, i);

        if (file_lock.l_type != F_UNLCK) {
            print_lock(file_lock);
        }
    }
    print_list(locks_list->next);
}

void read_char(int sys_file, off_t offset) {
    char buffer[2];
    CHECK_NEGATIVE_ONE("read_char", lseek(sys_file, offset, SEEK_SET), "Cannot find position in file")
    CHECK_NEGATIVE_ONE("read_char", read(sys_file, buffer, 1), "Cannot read character")
    printf("%s\n", buffer);
}

void write_char(int sys_file, off_t offset, char byte) {
    char buffer[] = {byte, 0};
    CHECK_NEGATIVE_ONE("write_char", lseek(sys_file, offset, SEEK_SET), "Cannot find position in file")
    CHECK_NEGATIVE_ONE("write_char", write(sys_file, buffer, 1), "Cannot read character")
    printf("Character written succesfully\n");
}

void REPL(int sys_file) {
    char *cmd = malloc(100);
    while (1) {
        scanf("%s", cmd);
        if (strcmp("exit", cmd) == 0)
            break;
        else if (strcmp("rl", cmd) == 0) {
            printf("Which byte?\n");
            unsigned int byte;
            scanf("%u", &byte);
            remove_lock_pos(locks_list, byte);
            push(locks_list, set_file_lock(F_SETLK, F_RDLCK, sys_file, byte));
            printf("Action successful\n");
        } else if (strcmp("rlw", cmd) == 0) {
            printf("Which byte?\n");
            unsigned int byte;
            scanf("%u", &byte);
            remove_lock_pos(locks_list, byte);
            push(locks_list, set_file_lock(F_SETLKW, F_RDLCK, sys_file, byte));
            printf("Action successful\n");
        } else if (strcmp("wl", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            remove_lock_pos(locks_list, byte);
            push(locks_list, set_file_lock(F_SETLK, F_WRLCK, sys_file, byte));
            printf("Action successful\n");
        } else if (strcmp("wlw", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            remove_lock_pos(locks_list, byte);
            push(locks_list, set_file_lock(F_SETLKW, F_WRLCK, sys_file, byte));
            printf("Action successful\n");
        } else if (strcmp("ls", cmd) == 0) {
            ls(sys_file);
            printf("Action successful\n");
        } else if (strcmp("drl", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            struct flock del;
            del.l_type = F_RDLCK;
            del.l_start = byte;
            if (remove_lock(locks_list, del)) {
                set_file_lock(F_SETLK, F_UNLCK, sys_file, byte);
                printf("Action successful\n");
            }
            else printf("Action failed\n");
        } else if (strcmp("dwl", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            struct flock del;
            del.l_type = F_WRLCK;
            del.l_start = byte;
            if(remove_lock(locks_list, del)) {
                set_file_lock(F_SETLK, F_UNLCK, sys_file, byte);
                printf("Action successful\n");
            }
            else printf("Action failed\n");
        } else if (strcmp("r", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            read_char(sys_file, byte);
        } else if (strcmp("w", cmd) == 0) {
            printf("Which byte?\n");
            int byte;
            scanf("%d", &byte);
            printf("Specify byte to write\n");
            int wr;
            scanf("%d", &wr);
            write_char(sys_file, byte, (char) wr);
        } else {
            help();
        }
    }
    free(cmd);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Wrong number of arguments\n");
        exit(-1);
    }
    char *file_name = argv[1];
    locks_list = create_empty();

    int sys_file;
    CHECK_NEGATIVE_ONE("main", sys_file = open(file_name, O_RDWR), "Cannot open file")

    REPL(sys_file);

    clear_list(locks_list);
}

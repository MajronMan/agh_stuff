#include "main.h"

int main(int argc, char *argv[]) {
    CHECK_ARGS(2);

    const char *file_name = argv[1];
    FILE *file;

    CHECK_NEGATIVE_ONE("main", access(argv[1], F_OK), "Cannot access file")
    CHECK_NULL("main", file = fopen(file_name, "r"), "Cannot open file")

    for (int i = 1; read_line(file, i); i++);

    fclose(file);
    return 0;
}

char is_empty(char *line) {
    for (int i = 0; line[i]; i++) if (!isspace(line[i])) return 0;
    return 1;
}

char read_line(FILE *file, int num) {
    char *line = NULL;
    size_t len = 0;
    ssize_t nread = getline(&line, &len, file);
    if (nread == -1) {
        CHECK_NON_ZERO("read_line", ferror(file), "Error while reading file")
        CHECK_ZERO("read_line", feof(file), "Unknown error")
        return 0;
    }
    if (nread > 0 && !is_empty(line)) {
        parse(line, num);
    }
    return 1;
}


void parse(char *line, int num) {
    line_data *data = calloc(1, sizeof(line_data));
    char *parts[20];
    char *part = strtok(line, PIPE);

    for (int i = 0; part != NULL; i++) {
        parts[i] = part;
        part = strtok(NULL, PIPE);
        data->command_count = i + 1;
    }
    for (int j = 0; j < data->command_count; j++) {
        data->argv[j][0] = strtok(parts[j], DELIMS);
        part = strtok(NULL, DELIMS);
        int k = 1;
        for (; part != NULL; k++) {
            data->argv[j][k] = part;
            part = strtok(NULL, DELIMS);
        }
        data->argc[j] = k;
    }

    printf("Interpreting line %d: \n", num);
    create_children(data);
    free(data);
}

void create_children(line_data *data) {
    pid_t pid;
    int pd[2];

    for (int i = 0; i < data->command_count; ++i) {
        pipe(pd);
        CHECK_NEGATIVE_ONE("create_children", pid = fork(), "Cannot create child")
        if (pid == 0) {
            if(i < data->command_count-1) {
                dup2(pd[WRITE], STDOUT_FILENO);
            }
            char msg[100];
            sprintf(msg, "Cannot execute %s", data->argv[i][0]);
            CHECK_NEGATIVE_ONE("create_children", execvp(data->argv[i][0], data->argv[i]), msg)
            exit(0);
        } else {
            int wstatus;
             dup2(pd[READ], STDIN_FILENO);
            waitpid(pid, &wstatus, 0);
            if(WIFEXITED(wstatus)){
                int exit_code = WEXITSTATUS(wstatus);
                char err[100] = "";
                sprintf(err, "Command %s exited with non-zero code %d", data->argv[i][0], exit_code);
                CHECK_NON_ZERO("create_children", exit_code, err)
            }

            close(pd[WRITE]);
        }
    }
}
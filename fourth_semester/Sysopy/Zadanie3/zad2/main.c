#include "main.h"

static struct rlimit memory_limit;
static struct rlimit time_limit;

int main(int argc, char *argv[]) {
    set_limits(argc, argv);

    const char *file_name = argv[1];
    FILE *file;

    CHECK_NEGATIVE_ONE("main", access(argv[1], F_OK), "Cannot access file")
    CHECK_NULL("main", file = fopen(file_name, "r"), "Cannot open file")

    for(int i=1; interpret_line(file, i); i++);

    fclose(file);
    return 0;
}

void set_limits(int argc, char **argv){
    switch(argc){
        case 2:
            CHECK_NEGATIVE_ONE("set_limits", getrlimit(RLIMIT_CPU, &time_limit), "Cannot get time limit")
            CHECK_NEGATIVE_ONE("set_limits", getrlimit(RLIMIT_AS, &memory_limit), "Cannot get memory limit")
            break;
        case 4:
            memory_limit.rlim_cur = memory_limit.rlim_max = (rlim_t) 1024*1024*atoll(argv[3]);
            time_limit.rlim_cur = time_limit.rlim_max = (rlim_t) atoll(argv[2]);
            break;
        default:
            HELP
            exit(-1);
    }
}

char interpret_line(FILE *file, int num){
    char *line = NULL;
    size_t len = 0;
    ssize_t nread = getline(&line, &len, file);
    if(nread == -1){
        CHECK_NON_ZERO("interpret_line", ferror(file), "Error while reading file")
        CHECK_ZERO("interpret_line", feof(file), "Unknown error")
        return 0;
    }
    if(nread > 0 && !is_empty(line)) {
        parse(line, num);
    }
    return 1;
}

void parse(char *line, int num){
    if(line[0] == '#'){
        const char *name = strtok(line+1, " \t\n\r");
        const char *value = strtok(NULL, "\n\r");
        if (value != NULL) {
            CHECK_NEGATIVE_ONE("parse", setenv(name, value, 0), "Failed to set environment variable")
        } else {
            CHECK_NEGATIVE_ONE("parse", unsetenv(name), "Failed to unset environment variable")
        }
    }
    else{
        char fun[100] = "";
        sprintf(fun, "parse %d", num);
        char *command = strtok(line, " \t\n\r");
        char *arg;
        int count = 1;
        char **argv = calloc(MAX_ARGS, sizeof(char*));
        CHECK_NULL(fun, argv, "Cannot allocate memory")
        argv[0] = command;
        while((arg = strtok(NULL, " \t\n\r")) != NULL && count < MAX_ARGS ){
            if(arg[0] == '$'){
                CHECK_NULL(fun, arg = getenv(arg + 1), "Cannot get environment variable")
            }
            argv[count++] = arg;
        }
        exec(argv, num);

        free(argv);
    }
}

void exec(char **argv, int num){
    char fun[100] = "";
    sprintf(fun, "exec %d", num);

    pid_t pid = fork();
    CHECK_NEGATIVE_ONE(fun, pid, "Failed to fork")

    if(pid == 0){
        char err[100] = "";
        sprintf(err, "Failed to execute command %s", argv[0]);
        CHECK_NEGATIVE_ONE(fun, setrlimit(RLIMIT_CPU, &time_limit), "Cannot set time limit")
        CHECK_NEGATIVE_ONE(fun, setrlimit(RLIMIT_AS, &memory_limit), "Cannot set memory limit")
        CHECK_NEGATIVE_ONE(fun, execvp(argv[0], argv), err)
    }
    else{
        int wstatus;
        rusage usage;
        wait4(pid, &wstatus, 0, &usage);
        if(WIFEXITED(wstatus)){
            int exit_code = WEXITSTATUS(wstatus);
            char err[100] = "";
            sprintf(err, "Command %s exited with non-zero code %d", argv[0], exit_code);

            CHECK_NON_ZERO(fun, exit_code, err)
            printf("Line %d executed properly\n", num);
        }else {
            printf("Line %d did not finish execution\n", num);
        }
        print_usage(usage, num, pid, argv);
    }
}

char is_empty(char *line){
    for(int i=0; line[i]; i++){
        if(!isspace(line[i]))
            return 0;
    }
    return 1;
}

void print_usage(rusage usage, int num, pid_t pid, char **argv) {
    printf("==========EXEC INFO==========\n");
    printf("Command: %s ", argv[0]);
    for(int i=1; argv[i]; i++)
        printf("%s ", argv[i]);
    printf("\nLine: %d\t\tpid: %d\n", num, pid);
    printf("Maximum resident memory: %6.likB\n", usage.ru_maxrss);
    printf("System time: %ld.%06ld\n", usage.ru_stime.tv_sec, usage.ru_stime.tv_usec);
    printf("User time: %ld.%06ld\n", usage.ru_utime.tv_sec, usage.ru_utime.tv_usec);
    printf("=============================\n");
}

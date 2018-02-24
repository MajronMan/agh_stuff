#include "main.h"

int main(int argc, char *argv[]) {
    if (argc != 2) {
        help();
        exit(-1);
    }

    const char *file_name = argv[1];
    FILE *file;

    CHECK_NEGATIVE_ONE("main", access(argv[1], F_OK), "Cannot access file")
    CHECK_NULL("main", file = fopen(file_name, "r"), "Cannot open file")

    for(int i=1; interpret_line(file, i); i++);

    fclose(file);
    return 0;
}


void help(){
    printf("Usage: ./main path/to/interpreted/file\n");
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
    CHECK_NEGATIVE_ONE(fun, execvp(argv[0], argv), err)
  }
  else{
    int wstatus;
    waitpid(pid, &wstatus, 0);
    if(WIFEXITED(wstatus)){
      int exit_code = WEXITSTATUS(wstatus);
      char err[100] = "";
      sprintf(err, "Command %s exited with non-zero code %d", argv[0], exit_code);
      CHECK_NON_ZERO(fun, exit_code, err)
    }
  }
}

char is_empty(char *line){
    for(int i=0; line[i]; i++){
        if(!isspace(line[i]))
            return 0;
    }
    return 1;
}

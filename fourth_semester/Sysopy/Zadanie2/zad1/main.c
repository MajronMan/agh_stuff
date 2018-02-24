#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <unistd.h>
#include <fcntl.h>

#define NUMBER "Wrong number of arguments"
#define NAME "Wrong task name"
#define LIB "Only sys or lib is allowed"
#define FILENAME "Cannot open file "
#define GENERATE "Generate takes only 4 arguments"
#define MALLOC "Cannot allocate memory"
#define SEEK "Seek failed"
#define WRONG(ERR) printf("%s\n", ERR); \
                    exit(-1);

#define CHECK_NULL(FUN, VAL, ERR) if((VAL) == NULL) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) if((VAL) == -1) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

struct chooser {
    int fun;
    char sys;
    char* file_name;
    int records_count;
    int records_size;
};

void sort(char sys, char *file_name, int records_count, int records_size);
void shuffle(char sys, char *file_name, int records_count, int records_size);
void generate(char sys, char *file_name, int records_count, int records_size);
void (*fun_pointers[])(char sys, char *file_name, int records_count, int records_size) = {&generate, &shuffle, &sort};

struct chooser parse(int argc, char **argv);

int main(int argc, char **argv) {
    srand((unsigned) time(NULL));
    
    struct chooser fargs = parse(argc, argv);
    (*fun_pointers[fargs.fun])(fargs.sys, fargs.file_name, fargs.records_count, fargs.records_size);

    return 0;
}

struct chooser parse(int argc, char **argv){
    if(argc != 5 && argc != 6) {
        WRONG(NUMBER);
    }
    if(strcmp(argv[1], "generate") == 0){
        if(argc == 6) {
            WRONG(GENERATE);
        }
        struct chooser c = {.fun = 0, .sys = 0, .file_name = argv[2], .records_count = atoi(argv[3]), .records_size = atoi(argv[4])};
        return c;
    }
    else if(argc == 5) {
        WRONG(NUMBER);
    }
    else if(strcmp(argv[2], "lib") != 0 && strcmp(argv[2], "sys") != 0){
        WRONG(LIB);
    }
    else if(strcmp(argv[1], "shuffle") == 0){
        struct chooser c = { 1, (char) strcmp(argv[2], "lib"), argv[3], atoi(argv[4]), atoi(argv[5])};
        return c;
    }
    else if(strcmp(argv[1], "sort") == 0){
        struct chooser c = {2, (char) strcmp(argv[2], "lib"), argv[3], atoi(argv[4]), atoi(argv[5])};
        return c;
    }
    WRONG(NAME);
}

void generate(char sys, char *file_name, int records_count, int records_size){
    char * random_name = "/dev/urandom";
    FILE *random = fopen(random_name, "r");
    CHECK_NULL("generate", random, FILENAME)

    FILE *file = fopen(file_name, "w");
    CHECK_NULL("generate", file, FILENAME)

    void *buffer = malloc((size_t) records_size);
    CHECK_NULL("generate", buffer, MALLOC)

    for (int i = 0; i < records_count; ++i) {
        size_t random_read = fread(buffer, (size_t) records_size, 1, random);
        fwrite(buffer, (size_t) records_size, random_read, file);
    }

    free(buffer);

    fclose(file);
    fclose(random);
}

int seek(char sys, int size, int sys_file, FILE *lib_file){
    if(sys){
        return (int) lseek(sys_file, size, SEEK_SET);
    }
    else{
        return fseek(lib_file, size, SEEK_SET);
    }
}
ssize_t my_read(char sys, void *buffer, int size, int sys_file, FILE *lib_file){
    if(sys){
        return read(sys_file, buffer, (size_t) size);
    }
    else {
        return fread(buffer, (size_t) size, 1, lib_file);
    }
}

void my_write(char sys, void *buffer, ssize_t size, int sys_file, FILE *lib_file){
    if(sys){
        write(sys_file, buffer, (size_t) size);
    }
    else {
        fwrite(buffer, (size_t) size, 1, lib_file);
    }
}

void swap(char sys, int records_size, int i, int j, int sys_file, FILE* lib_file){
    void *record_i = malloc((size_t) records_size);
    CHECK_NULL("swap", record_i, MALLOC)

    void *record_j = malloc((size_t) records_size);
    CHECK_NULL("swap", record_j, MALLOC)

    CHECK_NEGATIVE_ONE("swap", seek(sys, i*records_size, sys_file, lib_file), SEEK)
    ssize_t read_i = my_read(sys, record_i, records_size, sys_file, lib_file);
    CHECK_NEGATIVE_ONE("swap", seek(sys, j*records_size, sys_file, lib_file), SEEK)
    ssize_t read_j = my_read(sys, record_j, records_size, sys_file, lib_file);

    CHECK_NEGATIVE_ONE("swap", seek(sys, i*records_size, sys_file, lib_file), SEEK)
    my_write(sys, record_j, read_j, sys_file, lib_file);
    CHECK_NEGATIVE_ONE("swap", seek(sys, j*records_size, sys_file, lib_file), SEEK)
    my_write(sys, record_i, read_i, sys_file, lib_file);

    free(record_j);
    free(record_i);
}

void shuffle(char sys, char *file_name, int records_count, int records_size){
    int sys_file = -1;
    FILE *lib_file = NULL;
    if(sys) {
        sys_file = open(file_name, O_RDWR);
        CHECK_NEGATIVE_ONE("shuffle", sys_file, FILENAME)
    }
    else{
        lib_file = fopen(file_name, "r+");
        CHECK_NULL("shuffle", lib_file, FILENAME)
    }

    for(int i = records_count - 1, j = rand() % records_count; i > 0; i--){
        swap(sys, records_size, i, j, sys_file, lib_file);
    }

    if(sys){
        close(sys_file);
    }
    else {
        fclose(lib_file);
    }
}

void sort(char sys, char *file_name, int records_count, int records_size){
    int sys_file = -1;
    FILE *lib_file = NULL;
    if(sys) {
        sys_file = open(file_name, O_RDWR);
        CHECK_NEGATIVE_ONE("sort", sys_file, FILENAME)
    }
    else{
        lib_file = fopen(file_name, "r+");
        CHECK_NULL("sort", lib_file, FILENAME);
    }

    void *record_i = malloc((size_t) records_size);
    CHECK_NULL("sort", record_i, MALLOC)
    void *record_j = malloc((size_t) records_size);
    CHECK_NULL("sort", record_j, MALLOC)

    char swapped;
    do {
        swapped = 0;

        for (int i = 0; i < records_count - 1; ++i) {
            int j = i + 1;

            CHECK_NEGATIVE_ONE("sort", seek(sys, i*records_size, sys_file, lib_file), SEEK)
            ssize_t read_i = my_read(sys, record_i, records_size, sys_file, lib_file);
            CHECK_NEGATIVE_ONE("sort", seek(sys, j*records_size, sys_file, lib_file), SEEK)
            ssize_t read_j = my_read(sys, record_j, records_size, sys_file, lib_file);

            unsigned char key_i = ((unsigned char *) record_i)[0];
            unsigned char key_j = ((unsigned char *) record_j)[0];

            if (key_i > key_j) {
                CHECK_NEGATIVE_ONE("swap", seek(sys, i*records_size, sys_file, lib_file), SEEK)
                my_write(sys, record_j, read_j, sys_file, lib_file);
                CHECK_NEGATIVE_ONE("swap", seek(sys, j*records_size, sys_file, lib_file), SEEK)
                my_write(sys, record_i, read_i, sys_file, lib_file);

                swapped = 1;
            }
        }
    } while (swapped);

    free(record_i);
    free(record_j);
    if(sys){
        close(sys_file);
    }
    else{
        fclose(lib_file);
    }
}

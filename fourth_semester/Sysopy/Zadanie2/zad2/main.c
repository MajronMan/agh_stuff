#define _XOPEN_SOURCE 500
#include <ftw.h>
#include <dirent.h>

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <errno.h>

#define SLASH "/"
#define MAX 0

#define CHECK_NULL(FUN, VAL, ERR) if((VAL) == NULL) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) if((VAL) == -1) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}

void show_file_info(const char *fpath, const struct stat *sb) {
    printf("%s\t", fpath);
    printf( "%c%c%c%c%c%c%c%c%c%c\t",
            ((S_ISDIR(sb->st_mode))  ? 'd' : '-'),
            ((sb->st_mode & S_IRUSR) ? 'r' : '-'),
            ((sb->st_mode & S_IWUSR) ? 'w' : '-'),
            ((sb->st_mode & S_IXUSR) ? 'x' : '-'),
            ((sb->st_mode & S_IRGRP) ? 'r' : '-'),
            ((sb->st_mode & S_IWGRP) ? 'w' : '-'),
            ((sb->st_mode & S_IXGRP) ? 'x' : '-'),
            ((sb->st_mode & S_IROTH) ? 'r' : '-'),
            ((sb->st_mode & S_IWOTH) ? 'w' : '-'),
            ((sb->st_mode & S_IXOTH) ? 'x' : '-')
    );

    printf("%u bytes\t%s\t", (unsigned int)sb->st_size, ctime(&(sb->st_mtime)));
}

void check_file(const char *fpath, const struct stat *sb, unsigned int bytes){
    if (S_ISREG(sb->st_mode) && sb->st_size <= bytes) {
        show_file_info(fpath, sb);
    }
}

void add_to_path(char * path, char * name ){
    strcat(path, SLASH);
    strcat(path, name);
}

void search_stat(char* root_path, unsigned int bytes){
    char root_real[PATH_MAX];
    CHECK_NULL("search_stat", realpath(root_path, root_real), "Failed to determine real path")

    struct stat root_stat;
    CHECK_NEGATIVE_ONE("search_stat", lstat(root_real, &root_stat), "Failed to get stat")

    if(S_ISDIR(root_stat.st_mode)) {
        DIR *current = opendir(root_real);
        CHECK_NULL("search_stat", current, "Failed to open directory")

        struct dirent *explorer;
        while ((explorer = readdir(current)) != NULL) {
            if (strcmp(explorer->d_name, ".") == 0 || strcmp(explorer->d_name, "..") == 0 ){
                continue;
            }

            char child[PATH_MAX];
            strcpy(child, root_real);
            add_to_path(child, explorer->d_name);

            search_stat(child, bytes);
        }

        closedir(current);
    }
    else {
        check_file(root_real, &root_stat, bytes);
    }
}

unsigned int max_bytes = 0;

int visit_nftw(const char *fpath, const struct stat *sb, int tflag, struct FTW *ftwbuf){
    check_file(fpath, sb, max_bytes);
    return 0;
}

void search_nftw(char* root_path, unsigned int bytes){
    max_bytes = bytes;
    CHECK_NEGATIVE_ONE("search_nftw", nftw(root_path, visit_nftw , 20, FTW_DEPTH | FTW_PHYS), "Walk failed")
}

int main(int argc, char *argv[]) {
    if (argc != 4) {
        printf("Wrong number of arguments\n");
        return -1;
    }

    char *mode = argv[1];
    char *path = argv[2];
    unsigned int bytes = (unsigned int) atoi(argv[3]);

    if (strcmp(mode, "stat") == 0) {
        search_stat(path, bytes);
    }
    else if (strcmp(mode, "nftw") == 0) {
        search_nftw(path, bytes);
    } else {
        printf("Unknown command\n");
        return -1;
    }

    return 0;
}


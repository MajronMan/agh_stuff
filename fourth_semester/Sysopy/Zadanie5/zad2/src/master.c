#include "../include/master.h"

int main(int argc, char **argv){
    if(argc != 3){
        printf("Wrong number of arguments, expected 2\n");
        exit(-1);
    }
    char *path = argv[1];
    int R = atoi(argv[2]);
    int **T;
    FILE *file;
    char line[33];

    CHECK_ZERO("main",
               access(path, F_OK) != -1 || mkfifo(path, 0666) == 0,
               "Cannot acces nor create fifo")

    CHECK_NULL("init_array", T = calloc(R, sizeof(int *)), "Cannot allocate")
    for(int i=0; i<R; i++){
        CHECK_NULL("init_array", T[i] = calloc(R, sizeof(int)), "Cannot allocate")
    }
    CHECK_NULL("main", file = fopen(path, "r"),"Cannot open pipe")

    while(fgets(line, 33, file)){
        pixel read = read_pixel(line, R);
        T[read.x][read.y] = read.iters;
    }

    create_data_file(T, R);
    plot(R);
    for(int i=0; i<R; i++){
        free(T[i]);
    }
    free(T);

    fclose(file);
}

pixel read_pixel(char *line, int R) {
    double x, y;
    int iters;
    CHECK_ZERO("read_pixel", sscanf(line, "%lf %lf %d", &x, &y, &iters), "Cannot read line")
    int px = (int) ((x - RE_MIN) / (RE_MAX - RE_MIN) * R);
    int py = (int) ((y - IM_MIN) / (IM_MAX - IM_MIN) * R);
    pixel result = {px, py, iters};
    return result;
}
void create_data_file(int **T, int R) {
    FILE *data;
    CHECK_NULL("create_data_file", data = fopen(DATAFILE, "w"), "Cannot create datafile")
    for (int i = 0; i < R; i++) {
        for (int j = 0; j < R; j++) {
            fprintf(data, "%d %d %d\n", i, j, T[i][j]);
        }
    }
    fclose(data);
}
void plot(int R) {
    FILE *graph;
    CHECK_NULL("plot", graph = popen("gnuplot", "w"), "Cannot create a gnuplot")

    fprintf(graph,
            "%s\n"
                    "%s [0:%d]\n"
                    "%s[0:%d]\n"
                    "%s\n",
            "set view map",
            "set xrange", R,
            "set yrange", R,
            "plot 'data' with image"
    );

    fflush(graph);
    printf("Press enter to close\n");
    getc(stdin);

    pclose(graph);
}

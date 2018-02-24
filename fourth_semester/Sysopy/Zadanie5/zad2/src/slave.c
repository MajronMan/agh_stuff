#include "../include/slave.h"

int main(int argc, char **argv){
    if(argc != 4){
        printf("Wrong number of arguments, required 3\n");
        exit(-1);
    }
    srand((unsigned int) (time(NULL) ^ (getpid() << 16)));
    char *path = argv[1];
    int N = atoi(argv[2]), K = atoi(argv[3]);
    CHECK_ZERO("main",
               access(path, F_OK) != -1 || mkfifo(path, 0666) == 0,
               "Cannot acces nor create fifo")
    FILE *file = fopen(path, "w");

    CHECK_NULL("main", file, "Cannot open file")

    printf("Hi, this is slave %d and I'm gonna generate %d points at %d iterations\n", getpid(), N, K);
    generate_points(N, K, file);
    printf("%d finished generating\n", getpid());
    fclose(file);
}

int get_iters(complex c, int K) {
    complex prev = zero();
    for(int i=1; i<K; i++){
        if(mod(prev) > 2){
            return i;
        }
        prev = add(square(prev), c);
    }
    return K;
}

void generate_points(int N, int K, FILE *file) {
    for(int i=0; i<N; i++){
        double re = rand() / (double) RAND_MAX * (RE_MAX - RE_MIN) + RE_MIN;
        double im = rand() / (double) RAND_MAX * (IM_MAX - IM_MIN) + IM_MIN;
        complex point = {re, im};
        int iters = get_iters(point, K);
        fprintf(file, "% 10.8lf % 10.8lf %7.d\n", re, im, iters); //32 bytes
    }
}

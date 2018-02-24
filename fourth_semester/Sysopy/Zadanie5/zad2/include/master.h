#ifndef ZADANIE5_MAIN_H
#define ZADANIE5_MAIN_H

#define DATAFILE "./data"

#include "common.h"
typedef struct pixel{
    int x;
    int y;
    int iters;
} pixel;

pixel read_pixel(char *line, int R);
void create_data_file(int **T, int R);
void plot(int R);
#endif //ZADANIE5_MAIN_H

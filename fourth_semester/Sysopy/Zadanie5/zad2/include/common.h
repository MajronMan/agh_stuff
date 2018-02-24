#ifndef ZADANIE5_COMMON_H
#define ZADANIE5_COMMON_H
#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <wait.h>
#include <ctype.h>
#include <sys/resource.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define CHECK(FUN, VAL, COMP, ERR) if((VAL) == COMP) {\
    char msg[100];\
    sprintf(msg, "%s: %s\n", FUN, ERR);\
    perror(msg);\
    exit(-1);\
}
#define CHECK_NON_ZERO(FUN, VAL, ERR)if((VAL) != 0) {\
    printf("%s: %s\n", FUN, ERR);\
    exit(-1);\
}
#define CHECK_ZERO(FUN, VAL, ERR) CHECK(FUN, VAL, 0, ERR)
#define CHECK_NULL(FUN, VAL, ERR) CHECK(FUN, VAL, NULL, ERR)
#define CHECK_NEGATIVE_ONE(FUN, VAL, ERR) CHECK(FUN, VAL, -1, ERR)

#define RE_MIN -2
#define RE_MAX 1
#define IM_MIN -1
#define IM_MAX 1

typedef struct complex {
    double re;
    double im;
} complex;

complex square(complex a) {
    complex ret = { a.re * a.re - a.im * a.im, a.re * a.im * 2};
    return ret;
}

complex zero() {
    complex ret = {0, 0};
    return ret;
}

complex add(complex a, complex b) {
    complex ret = {a.re + b.re, a.im + b.im};
    return ret;
}

double mod(complex a) {
    return sqrt(a.re * a.re + a.im * a.im);
}


#endif //ZADANIE5_COMMON_H

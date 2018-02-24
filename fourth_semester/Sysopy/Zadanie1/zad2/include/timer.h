#ifndef ZAD1_TIMER_H
#define ZAD1_TIMER_H
#include <ctime>
#include <sys/resource.h>
#include <string>
#include <iostream>
#include "tree.h"
#include "list.h"

using namespace std;

typedef struct rusage rusage;
typedef struct timeval timeval;

class timer {
public:
    bool tree = false;
    string option;
    timer(bool t, string o){
        tree = t;
        option = o;
    }

    string getTimespecDiff(timespec start, timespec end);

    string getTimevalDiff(timeval start, timeval end);


    string getResults(timespec rStart, timespec rEnd, rusage start, rusage end) {

        string real = getTimespecDiff(rStart, rEnd);
        string user = getTimevalDiff(start.ru_utime, end.ru_utime);
        string system = getTimevalDiff(start.ru_stime, end.ru_stime);

        return "real: " + real + "\nuser: " + user + "\nsystem: "+ system + "\n";
    }

    void measure();
};


#endif //ZAD1_TIMER_H

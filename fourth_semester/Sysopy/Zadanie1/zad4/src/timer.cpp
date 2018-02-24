//
// Created by majronman on 19.03.17.
//

#include "../include/timer.h"
#include <cmath>

string timer::getTimespecDiff(timespec start, timespec end) {
    timeval vStart, vEnd;
    vStart.tv_sec = start.tv_sec;
    vStart.tv_usec = start.tv_nsec / 1000;

    vEnd.tv_sec = end.tv_sec;
    vEnd.tv_usec = end.tv_nsec / 1000;
    return getTimevalDiff(vStart, vEnd);
}

string timer::getTimevalDiff(timeval start, timeval end) {
    __time_t sec;
    __time_t usec;

    __suseconds_t delta_us = end.tv_usec - start.tv_usec;
    if (delta_us >= 0) {
        sec = end.tv_sec - start.tv_sec;
        usec = delta_us;
    } else {
        sec = end.tv_sec - start.tv_sec - 1;
        usec = 1000000 - delta_us;
    }
    int fsize = (int) (log(usec) / log(10));
    string zeros = "";
    for (int i = 0; i < 5 - fsize; i++)
        zeros += "0";

    return to_string(sec) + "." + zeros+ to_string(usec) + "s";
}

void timer::measure() {
    timespec rStart, rEnd;
    rusage start, end;

    Contact opt = Contact("A", "A", Date(0, 0, 0), "A", 0, "A");
    Contact pes = Contact("z", "z", Date(9999, 1, 1), "z", 999999999, "z");

    if(!tree){
        CBList *c = createContactsList(1000);
        if(option == "create"){
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            CBList *c2 = createContactsList(1000);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
            delete c2;
        }
        else if(option == "add"){
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->add(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->add(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "delete"){
            c->add(opt);
            c->add(pes);
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->remove(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->remove(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "find"){
            c->add(opt);
            c->add(pes);
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->find(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->find(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "sort"){
            cout<<"email:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->sort(2);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"phone:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->sort(3);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }

    }
    else{
        CBTree *c = createContactsTree(1000);
        if(option == "create"){
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            CBTree *c2 = createContactsTree(1000);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
            delete c2;
        }
        else if(option == "add"){
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->add(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->add(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "delete"){
            c->add(opt);
            c->add(pes);
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->remove(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->remove(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "find"){
            c->add(opt);
            c->add(pes);
            cout<<"optimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->find(opt);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"pessimistic:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->find(pes);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
        else if(option == "sort"){
            cout<<"email:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->sort(2);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            cout<<"phone:"<<endl;
            getrusage(RUSAGE_SELF, &start);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rStart);
            c->sort(3);
            clock_gettime(CLOCK_MONOTONIC_RAW, &rEnd);
            getrusage(RUSAGE_SELF, &end);
            cout<<getResults(rStart, rEnd, start, end);
            delete c;
        }
    }
}

extern "C" timer* createTimer(bool* b, string *arg2){
    return new timer(*b, *arg2);
}
extern "C" void destroyTimer( timer* object ) {
    delete object;
}

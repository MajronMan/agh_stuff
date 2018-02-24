#include "include/timer.h"
#include <ctime>
#include <cstdlib>
#include <dlfcn.h>

using namespace std;

int main(int argc, char** argv){
    srand((unsigned) time(NULL));
    srand((unsigned) time(NULL));
    string a1 = argv[1], a2 = argv[2];

#ifdef DYNAMIC
        void *symbols = dlopen("lib/libcontact.so", RTLD_LAZY);

        if (symbols == NULL) {
            cout<<"Cannot load dynamic library"<<endl;
            exit(1);
        }
        timer* (*createTimer)(bool*, string*);
        void (*destroy)(timer*);
        createTimer = (timer* (*)(bool*, string*))dlsym(symbols, "createTimer");
        destroy = (void (*)(timer*))dlsym(symbols, "destroyTimer");

        bool b = a1 == "tree";
        timer* tmr = createTimer(&b, &a2);
        tmr->tree = a1=="tree";
        tmr->option = a2;
        tmr->measure();
        destroy( tmr );
#else
        timer t = timer(a1 == "tree", a2);
        t.measure();
#endif
}

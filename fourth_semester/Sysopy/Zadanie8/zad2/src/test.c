#define _XOPEN_SOURCE 500
#include <signal.h>
#include <stdlib.h>

int main(){
  sigset_t sig_mask;
  sigfillset(&sig_mask);
  sigprocmask(SIG_BLOCK, &sig_mask, NULL);
  while(1) ;
}

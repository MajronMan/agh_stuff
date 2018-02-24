#include<stdlib.h>
#include <stdio.h>

int really_long = 1000000000;

int main(int argc, char **argv){
	if(argc > 1){
		printf("you don't have that much memory\n");				
		long long *big = calloc(really_long, sizeof(long long));
		for(int i=0; i<really_long; i++){
			big[i] = rand();
			if(i%1000==0) printf("Beep %d\n", i);
		}
	}
	else{
		printf("This will take a while\n");
		while(1);
	}
}

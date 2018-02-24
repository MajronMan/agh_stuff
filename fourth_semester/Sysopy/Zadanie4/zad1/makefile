MAIN=main.c
BINARY=main.out
TESTER=./test.txt

CC=gcc
CFLAGS= -Wall

all:
	$(CC) $(MAIN) -o $(BINARY) $(CFLAGS) 

run: $(MAIN)
	$(CC) $(MAIN) -o $(BINARY) $(CFLAGS) 
	./$(BINARY)

clean:
	rm $(BINARY)

.PHONY: run_test clean

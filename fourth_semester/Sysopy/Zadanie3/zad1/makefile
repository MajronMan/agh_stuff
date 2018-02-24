MAIN=main.c
BINARY=main.out
TESTER=./test.txt

CC=gcc
CFLAGS= -Wall

all:
	$(CC) $(MAIN) -o $(BINARY) $(CFLAGS) 

run_test: $(MAIN)
	./$(BINARY) $(TESTER)

clean:
	rm $(BINARY)

.PHONY: run_test clean

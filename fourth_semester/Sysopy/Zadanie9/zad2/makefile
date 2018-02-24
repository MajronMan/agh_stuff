
CC=gcc
CFLAGS=-Wall --std=c99 -g
LFLAGS=-lrt -lpthread

all:
	mkdir -p bin
	$(CC) src/main.c -o bin/main $(CFLAGS) $(LFLAGS)

clean:
	rm -rf bin

.PHONY: clean

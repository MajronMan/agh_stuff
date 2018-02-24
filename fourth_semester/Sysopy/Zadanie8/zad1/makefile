CC=gcc
CFLAGS=-Wall --std=c99 -g
LFLAGS=-lm -lpthread
FILES=main genfile
all:
	mkdir -p bin
	$(foreach FILE,$(FILES), \
	$(CC) src/$(FILE).c -o bin/$(FILE) $(CFLAGS) $(LFLAGS);\
	)

main:
	mkdir -p bin
	$(CC) src/main.c -o bin/main $(CFLAGS) $(LFLAGS)

genfile:
	mkdir -p bin
	$(CC) src/genfile.c -o bin/genfile $(CFLAGS) $(LFLAGS)

clean:
	rm -rf bin

.PHONY: main genfile clean


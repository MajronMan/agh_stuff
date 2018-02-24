CC=gcc
CFLAGS=-Wall --std=c99 -lrt
FILES=server client
all:
	$(foreach FILE,$(FILES), \
	$(CC) $(FILE).c -o $(FILE) $(CFLAGS);\
	)

run_server:
	$(CC) server.c -o server $(CFLAGS)
	./server

run_client:
	$(CC) client.c -o client $(CFLAGS)
	./client

clean:
	rm server
	rm client

.PHONY: run_test clean


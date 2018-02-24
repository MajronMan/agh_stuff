CC=gcc
CFLAGS=-Wall --std=c99
LFLAGS=-lpthread
FILES=server client
all:
	mkdir -p bin
	$(foreach FILE,$(FILES), \
	$(CC) src/$(FILE).c -o bin/$(FILE) $(CFLAGS) $(LFLAGS);\
	)

run_server:
	mkdir -p bin
	$(CC) src/server.c -o bin/server $(CFLAGS) $(LFLAGS)
	./bin/server 4040 path

run_client:
	mkdir -p bin
	$(CC) src/client.c -o bin/client $(CFLAGS) $(LFLAGS)
	./bin/client Namae inet 127.0.0.1:4040

clean:
	rm -rf bin

.PHONY: run_server run_client clean

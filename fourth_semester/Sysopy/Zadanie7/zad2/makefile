CC=gcc
CFLAGS=-Wall --std=c99
LFLAGS=-lrt -lpthread
FILES=barber customers
all:
	mkdir -p bin
	$(foreach FILE,$(FILES), \
	$(CC) src/$(FILE).c -o bin/$(FILE) $(CFLAGS) $(LFLAGS);\
	)

run_barber:
	mkdir -p bin
	$(CC) src/barber.c -o bin/barber $(CFLAGS) $(LFLAGS)
	./barber

run_customers:
	mkdir -p bin
	$(CC) src/customers.c -o bin/customers $(CFLAGS) $(LFLAGS)
	./customers

clean:
	rm -rf bin

.PHONY: run_barber run_customers clean


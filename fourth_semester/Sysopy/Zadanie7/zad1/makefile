CC=gcc
CFLAGS=-Wall --std=c99
FILES=barber customers
all:
	mkdir -p bin
	$(foreach FILE,$(FILES), \
	$(CC) src/$(FILE).c -o bin/$(FILE) $(CFLAGS);\
	)

run_barber:
	mkdir -p bin
	$(CC) src/barber.c -o bin/barber $(CFLAGS)
	./barber

run_customers:
	mkdir -p bin
	$(CC) src/customers.c -o bin/customers $(CFLAGS)
	./customers

clean:
	rm -rf bin

.PHONY: run_barber run_customers clean


MAIN=main.c
BINARY=main.out
FAIL=failer.c
FAILBIN=fail

TEST_MEM=./testmem.txt
TEST_TIME=./testtime.txt

CC=gcc
CFLAGS= -Wall

all:
	$(CC) $(FAIL) -o $(FAILBIN)
	$(CC) $(MAIN) -o $(BINARY) $(CFLAGS) 

run_test_mem: $(MAIN)
	./$(BINARY) $(TEST_MEM) 10 10

run_test_time: $(MAIN)
	./$(BINARY) $(TEST_TIME) 10 10

clean:
	rm $(BINARY) $(FAILBIN)

.PHONY: run_test_mem run_test_time clean


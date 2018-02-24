IDIR=include
EDIR=bin
SDIR=src

LPATH=$(shell pwd)/$(LDIR)

GCC=gcc
GCCFLAGS= --std=c99 -Wall -lm

all: master slave

master: $(SDIR)/master.c
	mkdir -p $(EDIR)
	$(GCC) -o $(EDIR)/$@.exe $< $(GCCFLAGS)

slave: $(SDIR)/slave.c
	mkdir -p $(EDIR)
	$(GCC) -o $(EDIR)/$@.exe $< $(GCCFLAGS)

run: master slave
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/slave.exe ./fifo 1000000 100 &
	./$(EDIR)/master.exe ./fifo 600 

clean:
	rm -rf $(EDIR) fifo data

.PHONY: clean


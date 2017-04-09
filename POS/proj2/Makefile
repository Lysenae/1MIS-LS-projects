# Projekt: POS #2 - Shell
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-04-09
# Soubor:  Makefile

CFLAGS=-std=gnu99 -Wall -Wextra -Werror -pedantic
LIBS=-lpthread
PROG=shell
FILES=monitor.o shell.o
CC=gcc

%.o : %.c
	$(CC) $(CFLAGS) -c $<

all: $(PROG)

.PHONY: clean

$(PROG): $(FILES)
	$(CC) $(CFLAGS) $(FILES) -o $(PROG) $(LIBS)

run:
	./ticket

clean:
	rm -f *~
	rm -f *.o
	rm -f $(PROG)
	rm -f $(PROG).out
	rm -f .fuse*

git: clean
	git add --all
	git commit
	git push -u origin master

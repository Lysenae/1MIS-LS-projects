# Projekt: POS #1 - Ticket algoritmus
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-02-19
# Soubor:  Makefile

PRE=-std=gnu99 -Wall -Wextra -Werror -pedantic
LIBS=-lpthread
PROG=ticket
MAIN=ticket

all:	$(MAIN)

.PHONY: clean

$(MAIN): $(MAIN).c
	gcc $(PRE) $(MAIN).c -o $(PROG) $(LIBS)

run:
	./ticket

clean:
	rm -f *~
	rm -f *.o
	rm -f $(PROG)
	rm -f $(PROG).out
	tm -f .fuse*

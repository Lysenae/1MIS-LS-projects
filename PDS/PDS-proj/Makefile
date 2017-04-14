# Projekt: PDS - L2 MitM
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-04-14
# Soubor:  Makefile

CXXFLAGS=-std=c++11 -Wall -Wextra -Werror -pedantic
PROG1=pds-scanner
FILES1=utils.o 
CXX=g++

all: $(PROG1)

utils.o: utils.c
	$(CXX) $(CXXFLAGS) -c utils.cpp

packet.o: packet.c
	$(CXX) $(CXXFLAGS) -c packet.c

arppkt.o: arppkt.c
	$(CXX) $(CXXFLAGS) -c arppkt.c

$(PROG): $(FILES1)
	$(CXX) $(CXXFLAGS) $(FILES1) -o $(PROG1)

clean:
	rm -f *~
	rm -f *.o
	rm -f $(PROG1)
	rm -f .fuse*

git: clean
	git add --all
	git commit
	git push -u origin master

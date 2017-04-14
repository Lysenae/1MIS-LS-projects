# Projekt: PDS - L2 MitM
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-04-14
# Soubor:  Makefile

CXXFLAGS=-std=c++11 -Wall -Wextra -Werror -pedantic
CXX=g++

PROG1=pds-scanner
FILES1= \
	utils.o \
	net.o \
	packet.o \
	arppkt.o \
	pds-scanner.o

all: $(PROG1)

utils.o: utils.cpp
	$(CXX) $(CXXFLAGS) -c utils.cpp

net.o: net.cpp
	$(CXX) $(CXXFLAGS) -c net.cpp

packet.o: packet.cpp
	$(CXX) $(CXXFLAGS) -c packet.cpp

arppkt.o: arppkt.cpp
	$(CXX) $(CXXFLAGS) -c arppkt.cpp

pds-scanner.o: pds-scanner.cpp
	$(CXX) $(CXXFLAGS) -c pds-scanner.cpp

$(PROG1): $(FILES1)
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

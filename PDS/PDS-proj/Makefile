# Projekt: PDS - L2 MitM
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz
# Datum:   2017-04-14
# Soubor:  Makefile

CXXFLAGS=-std=c++11 -Wall -Wextra -Werror -pedantic
CXX=g++

PROG1=pds-scanner
FILES1= \
	socket.o \
	macaddr.o \
	ipaddr.o \
	ipv4addr.o \
	ipv6addr.o \
	netitf.o \
	arppkt.o \
	pds-scanner.o

all: $(PROG1)

socket.o: socket.cpp
	$(CXX) $(CXXFLAGS) -c socket.cpp

macaddr.o: macaddr.cpp
	$(CXX) $(CXXFLAGS) -c macaddr.cpp

ipaddr.o: ipaddr.cpp
	$(CXX) $(CXXFLAGS) -c ipaddr.cpp

ipv4addr.o: ipv4addr.cpp
	$(CXX) $(CXXFLAGS) -c ipv4addr.cpp

ipv6addr.o: ipv6addr.cpp
	$(CXX) $(CXXFLAGS) -c ipv6addr.cpp

netitf.o: netitf.cpp
	$(CXX) $(CXXFLAGS) -c netitf.cpp

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
	rm -f *.cpp.*

git: clean
	git add --all
	git commit
	git push -u origin master

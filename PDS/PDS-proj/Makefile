# Projekt: PDS - L2 MitM
# Autor:   Daniel Klimaj, xklima22@stud.fit.vutbr.cz

AUTHOR=xklima22

CXXFLAGS=-std=c++11 -Wall -Werror -Wextra -pedantic
CXX=g++

PROG1=pds-scanner
PROG2=pds-spoof
PROG3=pds-intercept
FILES= \
	types.o \
	hash.o \
	hostgroup.o \
	macaddr.o \
	ipaddr.o \
	ipv4addr.o \
	ipv6addr.o \
	socket.o \
	netitf.o \
	packet.o \
	arppkt.o \
	icmpv6pkt.o \

all: $(PROG1) $(PROG2) $(PROG3)

types.o: types.cpp
	$(CXX) $(CXXFLAGS) -c types.cpp

hash.o: hash.cpp
	$(CXX) $(CXXFLAGS) -c hash.cpp

hostgroup.o: hostgroup.cpp
	$(CXX) $(CXXFLAGS) -c hostgroup.cpp

macaddr.o: macaddr.cpp
	$(CXX) $(CXXFLAGS) -c macaddr.cpp

ipaddr.o: ipaddr.cpp
	$(CXX) $(CXXFLAGS) -c ipaddr.cpp

ipv4addr.o: ipv4addr.cpp
	$(CXX) $(CXXFLAGS) -c ipv4addr.cpp

ipv6addr.o: ipv6addr.cpp
	$(CXX) $(CXXFLAGS) -c ipv6addr.cpp

socket.o: socket.cpp
	$(CXX) $(CXXFLAGS) -c socket.cpp

netitf.o: netitf.cpp
	$(CXX) $(CXXFLAGS) -c netitf.cpp

packet.o: packet.cpp
	$(CXX) $(CXXFLAGS) -c packet.cpp

arppkt.o: arppkt.cpp
	$(CXX) $(CXXFLAGS) -c arppkt.cpp

icmpv6pkt.o: icmpv6pkt.cpp
	$(CXX) $(CXXFLAGS) -c icmpv6pkt.cpp

pds-scanner.o: pds-scanner.cpp
	$(CXX) $(CXXFLAGS) -c pds-scanner.cpp -I/usr/include/libxml2/ -lxml2

pds-spoof.o: pds-spoof.cpp
	$(CXX) $(CXXFLAGS) -c pds-spoof.cpp

pds-intercept.o: pds-intercept.cpp
	$(CXX) $(CXXFLAGS) -c pds-intercept.cpp -I/usr/include/libxml2/ -lxml2

# --- Binarky ---

$(PROG1): $(FILES) $(PROG1).o
	$(CXX) $(CXXFLAGS) $(FILES) $(PROG1).o -o $(PROG1) -I/usr/include/libxml2/ -lxml2

$(PROG2): $(FILES) $(PROG2).o
	$(CXX) $(CXXFLAGS) $(FILES) $(PROG2).o -o $(PROG2)

$(PROG3): $(FILES) $(PROG3).o
	$(CXX) $(CXXFLAGS) $(FILES) $(PROG3).o -o $(PROG3) -I/usr/include/libxml2/ -lxml2

run-scan: $(PROG1)
	sudo ./pds-scanner -i eth0 -f devices.xml

run-spoof4: $(PROG2)
	sudo ./pds-spoof -i eth0 -t 5000 -p arp -victim1ip 10.190.8.76 -victim1mac 0800.2776.DE10 -victim2ip 10.190.11.57 -victim2mac 0800.2717.A19F

run-spoof6ll: $(PROG2)
	sudo ./pds-spoof -i eth0 -t 5000 -p ndp -victim1ip fe80::a00:27ff:fe76:de10 -victim1mac 0800.2776.DE10 -victim2ip fe80::a00:27ff:fe17:a19f -victim2mac 0800.2717.A19F

run-spoof6gl: $(PROG2)
	sudo ./pds-spoof -i eth0 -t 5000 -p ndp -victim1ip 2001:67c:1220:c1a0:ed4e:80b7:d968:27f -victim1mac 0800.2776.DE10 -victim2ip 2001:67c:1220:c1a0:741c:d41f:51b1:f49d -victim2mac 0800.2717.A19F

pack:
	zip $(AUTHOR).zip dokumentace.pdf *.cpp *.h README.txt Makefile

clean:
	rm -f *~
	rm -f *.o
	rm -f $(PROG1)
	rm -f $(PROG2)
	rm -f $(PROG3)
	rm -f .fuse*
	rm -f *.cpp.*
	rm -f *.xml
	rm -f $(AUTHOR).zip

git: clean
	git add --all
	git commit
	git push -u origin master

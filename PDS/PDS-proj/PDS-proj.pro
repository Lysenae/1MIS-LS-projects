TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    packet.cpp \
    arppkt.cpp \
    pds-scanner.cpp \
    net.cpp \
    ipv4addr.cpp \
    ipv6addr.cpp \
    ipaddr.cpp

HEADERS += \
    packet.h \
    arppkt.h \
    net.h \
    ipv4addr.h \
    ipv6addr.h \
    types.h \
    ipaddr.h

DISTFILES += \
    Makefile

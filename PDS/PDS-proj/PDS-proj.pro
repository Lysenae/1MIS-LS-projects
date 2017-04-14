TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    packet.cpp \
    arppkt.cpp \
    utils.cpp \
    pds-scanner.cpp \
    net.cpp

HEADERS += \
    packet.h \
    arppkt.h \
    utype.h \
    utils.h \
    net.h

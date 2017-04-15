#ifndef MACADDR_H
#define MACADDR_H

#include <cstdio>

#include <sys/ioctl.h>
#include <net/if.h>

#include "types.h"
#include "socket.h"

class MACAddr
{
public:
    static const uint MAC_BLOCKS = 6;

    MACAddr(ifreq *ifr);
    std::string to_string() const;

private:
    uchar m_mac[6];
};

#endif // MACADDR_H

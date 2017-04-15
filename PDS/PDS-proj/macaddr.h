#ifndef MACADDR_H
#define MACADDR_H

#include <sys/ioctl.h>
#include <net/if.h>

#include "types.h"
#include "socket.h"

class MACAddr
{
public:
    MACAddr(struct ifreq ifr);

private:
    uchar m_mac[6];
};

#endif // MACADDR_H

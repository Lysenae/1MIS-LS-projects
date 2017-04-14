#ifndef IPV4ADDR_H
#define IPV4ADDR_H

#include "netaddr.h"

class IPv4Addr : public NetAddr
{
public:
    IPv4Addr(struct ifaddrs *ifa);
};

#endif // IPV4ADDR_H

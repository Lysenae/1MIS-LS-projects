#ifndef IPV6ADDR_H
#define IPV6ADDR_H

#include <vector>

#include "netaddr.h"

class IPv6Addr : public NetAddr
{
public:
    IPv6Addr(ifaddrs *ifa);
};

typedef std::vector<IPv6Addr *> IPv6Vect;

#endif // IPV6ADDR_H

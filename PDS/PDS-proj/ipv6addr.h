#ifndef IPV6ADDR_H
#define IPV6ADDR_H

#include <vector>

#include "netaddr.h"

class IPv6Addr : public NetAddr
{
public:
    static const uint IPV6_BLOCKS = 8;

    IPv6Addr(ifaddrs *ifa);

private:
    virtual std::string get_group(std::string ins, uint idx) override;
};

typedef std::vector<IPv6Addr *> IPv6Vect;

#endif // IPV6ADDR_H

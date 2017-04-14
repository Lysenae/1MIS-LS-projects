#ifndef IPV6ADDR_H
#define IPV6ADDR_H

#include <vector>

#include "netaddr.h"

class IPv6Addr : public NetAddr
{
public:
    IPv6Addr(ifaddrs *ifa);
    std::string addr_grp(uint idx);
    std::string mask_grp(uint idx);

private:
    static const uint IPV6_BLOCKS = 8;
    std::string get_group(std::string s, uint idx);
};

typedef std::vector<IPv6Addr *> IPv6Vect;

#endif // IPV6ADDR_H

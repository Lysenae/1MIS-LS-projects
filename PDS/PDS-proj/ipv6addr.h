#ifndef IPV6ADDR_H
#define IPV6ADDR_H

#include <vector>

#include "ipaddr.h"

class IPv6Addr : public IPAddr
{
public:
    static const uint BLOCKS = 8;

    IPv6Addr(ifaddrs *ifa);

private:
    virtual std::string get_group(std::string ins, uint idx) override;
};

typedef std::vector<IPv6Addr *> IPv6Vect;

#endif // IPV6ADDR_H

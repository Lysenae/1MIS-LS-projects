#ifndef IPV4ADDR_H
#define IPV4ADDR_H

#include "netaddr.h"

class IPv4Addr : public NetAddr
{
public:
    IPv4Addr(struct ifaddrs *ifa);

private:
    static const uint IPV4_BLOCKS = 4;
    virtual std::string get_group(std::string ins, uint idx) override;
};

#endif // IPV4ADDR_H

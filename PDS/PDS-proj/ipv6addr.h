#ifndef IPV6ADDR_H
#define IPV6ADDR_H

#include <iostream>

#include "ipaddr.h"
#include "types.h"

class IPv6Addr : public IPAddr
{
public:
    static const uint BLOCKS = 8;
    static const uint GRP_S  = 4;

    IPv6Addr(ifaddrs *ifa);
    ~IPv6Addr() {}
    virtual std::string addr_grp(uint idx) override;
    virtual std::string mask_grp(uint idx) override;
    UchrVect to_uchar();

private:
    virtual std::string get_group(std::string ins, uint idx) override;
    uchar literal_to_uchr(std::string s);
};

typedef std::vector<IPv6Addr *> IPv6Vect;

#endif // IPV6ADDR_H

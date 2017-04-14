#ifndef IPV4ADDR_H
#define IPV4ADDR_H

#include "netaddr.h"

class IPv4Addr : public NetAddr
{
public:
    static const uint IPV4_BLOCKS = 4;
    static const uint IPV4_BITS   = 32;

    IPv4Addr(struct ifaddrs *ifa);
    int mask_n();
    StrVect net_host_ips();

private:
    virtual std::string get_group(std::string ins, uint idx) override;
    int get_addr_n(uint idx);
    int uchb(uchar uc);
    int maxval(uint bits);
    std::string addr_part(uint until_octet);
    StrVect expand_ips(StrVect ips, int max, int base, int octet);
    StrVect remove_bc_net(StrVect ips);
};

#endif // IPV4ADDR_H

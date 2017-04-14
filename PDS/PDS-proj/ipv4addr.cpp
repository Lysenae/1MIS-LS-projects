#include "ipv4addr.h"

IPv4Addr::IPv4Addr(ifaddrs *ifa) : NetAddr(ifa, IPVer::IPV4)
{
}

std::string IPv4Addr::get_group(std::string ins, uint idx)
{
    std::string s = "";
    if(idx < IPV4_BLOCKS)
    {
        StrVect grps = split_addr(ins, '.');
        s = grps[IPV4_BLOCKS - idx - 1];
    }
    return s;
}

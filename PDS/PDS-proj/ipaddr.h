#ifndef IPADDR_H
#define IPADDR_H

#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <iostream>

#include "types.h"

enum class IPVer
{
    IPV4,
    IPV6
};

class IPAddr
{
public:
    IPAddr(ifaddrs *ifa, IPVer v);
    IPAddr(std::string ip, std::string mask);
    std::string interface();
    std::string addr();
    std::string snmask();
    std::string addr_grp(uint idx);
    std::string mask_grp(uint idx);

protected:
    std::string m_itf_name;
    std::string m_addr;
    std::string m_mask;

    StrVect split_addr(std::string s, char delimiter);
    virtual std::string get_group(std::string ins, uint idx) = 0;
};

#endif // IPADDR_H

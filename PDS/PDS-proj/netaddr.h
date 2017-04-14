#ifndef NETADDR_H
#define NETADDR_H

#include <string>
#include <vector>

#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "utype.h"

enum class IPVer
{
    IPV4,
    IPV6
};

class NetAddr
{
public:
    NetAddr(ifaddrs *ifa, IPVer v);
    std::string interface();
    std::string addr();
    std::string snmask();

protected:
    std::string m_itf_name;
    std::string m_addr;
    std::string m_mask;

    std::vector<std::string> split_addr(std::string s, char delimiter);
};

#endif // NETADDR_H

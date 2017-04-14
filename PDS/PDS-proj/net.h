#ifndef NET_H
#define NET_H

#include <string>
#include <vector>

#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "ipv4addr.h"
#include "ipv6addr.h"

class Net
{
public:
    Net();
    IPv4Addr *ipv4(std::string interface);
    IPv6Vect ipv6(std::string interface);

private:
    struct ifaddrs *m_ifaddrs;
    struct ifaddrs *m_ifa_next;
    void *m_tmp_addr;
};

#endif // NET_H

#ifndef NETITF_H
#define NETITF_H

#include <string>
#include <vector>
#include <cstring>
#include <iostream>

#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/ethernet.h>
#include <linux/if_packet.h>

#include "ipv4addr.h"
#include "ipv6addr.h"
#include "socket.h"
#include "macaddr.h"

class NetItf
{
public:
    NetItf(std::string interface);
    ~NetItf();
    IPv4Addr *ipv4();
    IPv6Vect ipv6();
    MACAddr *mac();
    int index();

private:
    std::string m_interface;
    struct ifaddrs *m_ifaddrs;
    struct ifaddrs *m_ifa_next;
    struct ifreq *m_ifr;
    void *m_tmp_addr;
    Socket *m_sock;

    void free_ifr();
    void free_ifaddr();
};

#endif // NETITF_H

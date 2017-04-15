#ifndef NET_H
#define NET_H

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

class Net
{
public:
    Net(std::string interface);
    IPv4Addr *ipv4();
    IPv6Vect ipv6();
    int if_index();

private:
    std::string m_interface;
    struct ifaddrs *m_ifaddrs;
    struct ifaddrs *m_ifa_next;
    void *m_tmp_addr;
};

#endif // NET_H

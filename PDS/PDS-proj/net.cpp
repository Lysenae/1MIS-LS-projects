#include "net.h"

Net::Net(std::string interface)
{
    m_interface = interface;
    m_ifaddrs   = nullptr;
    m_ifa_next  = nullptr;
    m_tmp_addr  = nullptr;
}

IPv4Addr *Net::ipv4()
{
    IPv4Addr *ip = nullptr;
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET &&
        m_interface == std::string(m_ifa_next->ifa_name))
        {
            ip = new IPv4Addr(m_ifa_next);
        }
    }
    return ip;
}

IPv6Vect Net::ipv6()
{
    IPv6Vect v;
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET6 &&
        m_interface == std::string(m_ifa_next->ifa_name))
            v.push_back(new IPv6Addr(m_ifa_next));
    }
    return v;
}

int Net::if_index()
{
    struct ifreq ifr;
    Socket s(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if(s.open() != SocketStatus::OPENED)
        return OP_FAIL;
    strncpy(ifr.ifr_name, m_interface.c_str(), IFNAMSIZ);
    if(ioctl(s.fd(), SIOCGIFINDEX, &ifr) == -1)
        return OP_FAIL;
    s.close();
    return ifr.ifr_ifindex;
}

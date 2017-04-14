#include "net.h"

Net::Net()
{
    m_ifaddrs  = nullptr;
    m_ifa_next = nullptr;
    m_tmp_addr = nullptr;
}

IPv4Addr *Net::ipv4(std::string interface)
{
    IPv4Addr *ip = nullptr;
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET &&
        interface == std::string(m_ifa_next->ifa_name))
        {
            ip = new IPv4Addr(m_ifa_next);
        }
    }
    return ip;
}

IPv6Vect Net::ipv6(std::string interface)
{
    IPv6Vect v;
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET6 &&
        interface == std::string(m_ifa_next->ifa_name))
            v.push_back(new IPv6Addr(m_ifa_next));
    }
    return v;
}

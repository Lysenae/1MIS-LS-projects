#include "netitf.h"

NetItf::NetItf(std::string interface)
{
    m_interface = interface;
    m_ifaddrs   = nullptr;
    m_ifa_next  = nullptr;
    m_tmp_addr  = nullptr;
    m_sock      = new Socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
}

NetItf::~NetItf()
{
    if(m_ifaddrs != nullptr)
    {
        delete m_ifaddrs;
        m_ifaddrs = nullptr;
    }
    if(m_ifa_next != nullptr)
    {
        delete m_ifa_next;
        m_ifa_next = nullptr;
    }
    m_tmp_addr = nullptr;
    if(m_sock->status() == SocketStatus::OPENED)
    {
        m_sock->close();
        delete m_sock;
        m_sock = nullptr;
    }
}

IPv4Addr *NetItf::ipv4()
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

IPv6Vect NetItf::ipv6()
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

MACAddr *NetItf::mac()
{
    Socket s(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    return nullptr;
}

int NetItf::if_index()
{
    Socket s(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if(s.open() != SocketStatus::OPENED)
        return OP_FAIL;
    strncpy(m_ifr.ifr_name, m_interface.c_str(), IFNAMSIZ);
    if(ioctl(s.fd(), SIOCGIFINDEX, &m_ifr) == -1)
        return OP_FAIL;
    s.close();
    return m_ifr.ifr_ifindex;
}
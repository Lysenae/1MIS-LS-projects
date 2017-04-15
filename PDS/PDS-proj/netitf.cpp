#include "netitf.h"

NetItf::NetItf(std::string interface)
{
    m_interface = interface;
    m_ifaddrs   = nullptr;
    m_ifa_next  = nullptr;
    m_tmp_addr  = nullptr;
    m_ifr       = nullptr;
    m_sock      = new Socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
}

NetItf::~NetItf()
{
    free_ifaddr();
    free_ifr();
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
    free_ifaddr();
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
    free_ifaddr();
    return v;
}

MACAddr *NetItf::mac()
{

    return nullptr;
}

int NetItf::index()
{
    int idx = OP_FAIL;
    m_ifr = new ifreq;
    if(m_sock->open() != SocketStatus::OPENED)
        return OP_FAIL;
    strncpy(m_ifr->ifr_name, m_interface.c_str(), IFNAMSIZ);
    if(ioctl(m_sock->fd(), SIOCGIFINDEX, m_ifr) == OP_FAIL)
        return OP_FAIL;
    m_sock->close();
    idx = m_ifr->ifr_ifindex;
    free_ifr();
    return idx;
}

void NetItf::free_ifr()
{
    if(m_ifr != nullptr)
    {
        delete m_ifr;
        m_ifr = nullptr;
    }
}

void NetItf::free_ifaddr()
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
}

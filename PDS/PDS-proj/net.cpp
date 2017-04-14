#include "net.h"

Net::Net()
{
    m_ifaddrs  = nullptr;
    m_ifa_next = nullptr;
    m_tmp_addr = nullptr;
}

std::string Net::ipv4(std::string interface)
{
    std::string ip = "";
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET)
        {
            m_tmp_addr = &((struct sockaddr_in *)m_ifa_next->ifa_addr)->sin_addr;
            char addr_buff[INET_ADDRSTRLEN];
            inet_ntop(AF_INET, m_tmp_addr, addr_buff, INET_ADDRSTRLEN);
            if(interface == std::string(m_ifa_next->ifa_name))
            {
                ip = std::string(addr_buff);
                break;
            }
        }
    }
    return ip;
}

std::vector<std::string> Net::ipv6(std::string interface)
{
    std::vector<std::string> v;
    getifaddrs(&m_ifaddrs);

    for (m_ifa_next = m_ifaddrs; m_ifa_next != nullptr;
    m_ifa_next = m_ifa_next->ifa_next)
    {
        if(!m_ifa_next->ifa_addr)
            continue;

        if(m_ifa_next->ifa_addr->sa_family == AF_INET6)
        {
            m_tmp_addr = &((struct sockaddr_in6 *)m_ifa_next->ifa_addr)->sin6_addr;
            char addr_buff[INET6_ADDRSTRLEN];
            inet_ntop(AF_INET6, m_tmp_addr, addr_buff, INET6_ADDRSTRLEN);
            if(interface == std::string(m_ifa_next->ifa_name))
                v.push_back(std::string(addr_buff));
        }
    }
    return v;
}

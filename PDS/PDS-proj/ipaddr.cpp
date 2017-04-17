#include "ipaddr.h"

IPAddr::IPAddr(IPVer v, ifaddrs *ifa)
{
    sockaddr_in  *in4 = nullptr;
    sockaddr_in6 *in6 = nullptr;

    m_itf_name = ifa->ifa_name;
    char *addr = new char[INET6_ADDRSTRLEN];
    char *mask = new char[INET6_ADDRSTRLEN];
    memset(addr, 0, INET6_ADDRSTRLEN);
    memset(mask, 0, INET6_ADDRSTRLEN);

    if(v == IPVer::IPV4)
    {
        in4 = (struct sockaddr_in*) ifa->ifa_addr;
        inet_ntop(AF_INET, &in4->sin_addr, addr, INET_ADDRSTRLEN);
        in4 = (struct sockaddr_in*) ifa->ifa_netmask;
        inet_ntop(AF_INET, &in4->sin_addr, mask, INET_ADDRSTRLEN);
    }
    else
    {
        in6 = (struct sockaddr_in6*) ifa->ifa_addr;
        inet_ntop(AF_INET6, &in6->sin6_addr, addr, INET6_ADDRSTRLEN);
        in6 = (struct sockaddr_in6*) ifa->ifa_netmask;
        inet_ntop(AF_INET6, &in6->sin6_addr, mask, INET6_ADDRSTRLEN);
    }

    m_addr = std::string(addr);
    m_mask = std::string(mask);
}

IPAddr::IPAddr(IPVer v, std::string ip, std::string mask)
{
    sockaddr_in  sa4;
    sockaddr_in6 sa6;
    int rslt;

    if(v == IPVer::IPV4)
        rslt = inet_pton(AF_INET, ip.c_str(), &(sa4.sin_addr));
    else
        rslt = inet_pton(AF_INET6, ip.c_str(), &(sa6.sin6_addr));
    if(rslt != 0)
        m_addr = ip;
    else
    {
        std::cerr << "IPAddr constructor: Invalid IP address" << std::endl;
        m_addr = "";
    }

    if(v == IPVer::IPV4)
        rslt = inet_pton(AF_INET, mask.c_str(), &(sa4.sin_addr));
    else
    {
        rslt = inet_pton(AF_INET6, mask.c_str(), &(sa6.sin6_addr));
    }
    if(rslt != 0)
        m_mask = mask;
    else
    {
        std::cerr << "IPAddr constructor: Invalid subnet mask" << std::endl;
        m_addr = "";
    }

    m_itf_name = "";
}

std::string IPAddr::interface()
{
    return m_itf_name;
}

std::string IPAddr::addr()
{
    return m_addr;
}

std::string IPAddr::snmask()
{
    return m_mask;
}

StrVect IPAddr::split_addr(std::string s, char delimiter)
{
    StrVect v;
    std::string t = "";
    for(uint i=0; i<s.size(); ++i)
    {
        if(s[i] == delimiter)
        {
            v.push_back(t);
            t = "";
        }
        else
        {
            t += s[i];
            if(i == s.size()-1)
                v.push_back(t);
        }
    }
    return v;
}


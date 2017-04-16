#include "ipaddr.h"

IPAddr::IPAddr(ifaddrs *ifa, IPVer v)
{
    uint len = v == IPVer::IPV4 ? INET_ADDRSTRLEN : INET6_ADDRSTRLEN;
    int ver  = v == IPVer::IPV4 ? AF_INET         : AF_INET6;

    m_itf_name = ifa->ifa_name;
    void *tmp_ptr = nullptr;
    char *buff = new char[len];

    tmp_ptr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
    inet_ntop(ver, tmp_ptr, buff, len);
    m_addr = std::string(buff);

    tmp_ptr = &((struct sockaddr_in *)ifa->ifa_netmask)->sin_addr;
    inet_ntop(ver, tmp_ptr, buff, len);
    m_mask = std::string(buff);
}

IPAddr::IPAddr(std::string ip, std::string mask)
{
    struct sockaddr_in sa;
    int rslt;

    rslt = inet_pton(AF_INET, ip.c_str(), &(sa.sin_addr));
    if(rslt != 0)
        m_addr = ip;
    else
    {
        std::cerr << "IPAddr constructor: Invalid IP address" << std::endl;
        m_addr = "";
    }

    rslt = inet_pton(AF_INET, mask.c_str(), &(sa.sin_addr));
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


#include "netaddr.h"

NetAddr::NetAddr(ifaddrs *ifa, IPVer v)
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

std::string NetAddr::interface()
{
    return m_itf_name;
}

std::string NetAddr::addr()
{
    return m_addr;
}

std::string NetAddr::snmask()
{
    return m_mask;
}

std::string NetAddr::addr_grp(uint idx)
{
    return get_group(m_addr, idx);
}

std::string NetAddr::mask_grp(uint idx)
{
    return get_group(m_mask, idx);
}

StrVect NetAddr::split_addr(std::string s, char delimiter)
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


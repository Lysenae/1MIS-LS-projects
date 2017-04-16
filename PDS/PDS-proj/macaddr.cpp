#include "macaddr.h"

MACAddr::MACAddr(ifreq *ifr)
{
    for (uint i=0; i < OCTETS; ++i)
        m_mac[i] = ifr->ifr_hwaddr.sa_data[i];
}

MACAddr::MACAddr(std::vector<uchar> oct)
{
    if(oct.size() == OCTETS)
    {
        for(uint i=0; i<OCTETS; ++i)
            m_mac[i] = oct[i];
    }
    else
    {
        for(uint i=0; i<OCTETS; ++i)
            m_mac[i] = 0x00;
    }
}

std::string MACAddr::to_string() const
{
    char buffer[20];
    sprintf(buffer, "%02X:%02X:%02X:%02X:%02X:%02X",
        m_mac[0],m_mac[1],m_mac[2],m_mac[3],m_mac[4],m_mac[5]);
    return std::string(buffer);
}

uchar MACAddr::octet(uint idx)
{
    if(idx < OCTETS)
        return m_mac[idx];
    return (uchar) 0;
}

bool MACAddr::eq(MACAddr *other)
{
    for(uint i=0; i<OCTETS; ++i)
    {
        if(m_mac[i] != other->octet(i))
            return false;
    }
    return true;
}

bool MACAddr::empty() const
{
    for(uint i=0; i<OCTETS; ++i)
    {
        if(m_mac[i] != 0x00)
            return false;
    }
    return true;
}

#include "macaddr.h"

MACAddr::MACAddr(ifreq *ifr)
{
    for (uint i=0; i < OCTETS; ++i)
        m_mac[i] = ifr->ifr_hwaddr.sa_data[i];
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

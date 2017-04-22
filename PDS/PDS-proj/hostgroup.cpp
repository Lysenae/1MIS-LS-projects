#include "hostgroup.h"

HostGroup::HostGroup(std::string id, std::string mac1, std::string mac2)
{
    m_id       = id;
    m_pairs    = 0;
    m_mac1     = mac1;
    m_mac2     = mac2;
    m_buff_idx = -1;
}

std::string HostGroup::id()
{
    return m_id;
}

StrVect HostGroup::ip_pair(uint idx)
{
    StrVect p;
    if(idx < m_pairs)
    {
        p.push_back(m_host1[idx]);
        p.push_back(m_host2[idx]);
    }
    return p;
}

std::string HostGroup::mac(uint host)
{
    std::string r = "";
    if(host == 1)
        r = m_mac1;
    else if(host == 2)
        r = m_mac2;
    return r;
}

void HostGroup::add_ip_pair(std::string ip1, std::string ip2)
{
    m_host1.push_back(ip1);
    m_host2.push_back(ip2);
    m_pairs += 1;
}

uint HostGroup::size()
{
    return m_pairs;
}

void HostGroup::print()
{
    std::cout << "MAC 1: " << m_mac1 << std::endl;
    for(std::string s : m_host1)
        std::cout << "\t" << s << std::endl;
    std::cout << "MAC 2: " << m_mac2 << std::endl;
    for(std::string s : m_host2)
        std::cout << "\t" << s << std::endl;
}

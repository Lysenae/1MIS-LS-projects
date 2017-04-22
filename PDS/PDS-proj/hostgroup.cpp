#include "hostgroup.h"

HostGroup::HostGroup(StrVect h1, StrVect h2)
{
    m_id    = find_prop(h1, "group");
    m_mac1  = new MACAddr(find_prop(h1, "mac"));
    m_mac2  = new MACAddr(find_prop(h2, "mac"));
    m_ip4_1 = new IPv4Addr(find_prop(h1, "ipv4"));
    m_ip4_2 = new IPv4Addr(find_prop(h2, "ipv4"));
    StrVect h1_v6 = find_ipv6s(h1);
    StrVect h2_v6 = find_ipv6s(h2);

    if(h1_v6.size() == h2_v6.size())
    {
        for(uint i=0; i<h1_v6.size(); ++i)
            add_ip6_pair(h1_v6[i], h2_v6[i]);
    }
}

std::string HostGroup::id()
{
    return m_id;
}

MACAddr *HostGroup::mac1()
{
    return m_mac1;
}

MACAddr *HostGroup::mac2()
{
    return m_mac2;
}

IPv4Addr *HostGroup::ipv4_1()
{
    return m_ip4_1;
}

IPv4Addr *HostGroup::ipv4_2()
{
    return m_ip4_2;
}

IPv6Addr *HostGroup::ipv6_1(uint idx)
{
    if(idx < m_ip6s_1.size())
        return m_ip6s_1[idx];
    return nullptr;
}

IPv6Addr *HostGroup::ipv6_2(uint idx)
{
    if(idx < m_ip6s_2.size())
        return m_ip6s_2[idx];
    return nullptr;
}

void HostGroup::add_ip6_pair(std::string ip1, std::string ip2)
{
    m_ip6s_1.push_back(new IPv6Addr(ip1));
    m_ip6s_2.push_back(new IPv6Addr(ip2));
    m_pairs += 1;
}

uint HostGroup::size()
{
    return m_pairs;
}

void HostGroup::print()
{
    std::cout << "Group: " << m_id << std::endl;
    std::cout << "  MAC 1: " << m_mac1->to_string() << std::endl;
    std::cout << "    IPv4: " << m_ip4_1->addr() << std::endl;
    for(IPv6Addr *a : m_ip6s_1)
        std::cout << "    IPv6: " << a->addr() << std::endl;
    std::cout << "  MAC 2: " << m_mac2->to_string() << std::endl;
    std::cout << "    IPv4: " << m_ip4_2->addr() << std::endl;
    for(IPv6Addr *a : m_ip6s_2)
        std::cout << "    IPv6: " << a->addr() << std::endl;
    std::cout << std::endl;
}

StrVect HostGroup::find_ipv6s(StrVect inv)
{
    StrVect v6;
    for(std::string s : inv)
    {
        StrVect vt = split_str(s, '@');
        if(vt.size() == 2 && vt[0] == "ipv6")
            v6.push_back(vt[1]);
    }
    return v6;
}

std::string HostGroup::find_prop(StrVect inv, std::string what)
{
    std::string p = "";
    for(std::string s : inv)
    {
        StrVect vt = split_str(s, '@');
        if(vt.size() == 2 && vt[0] == what)
            p = vt[1];
    }
    return p;
}

#include "hostgroup.h"

HostGroup::HostGroup(std::string id, StrVect h1, StrVect h2)
{
    m_id = id;
    m_mac1 = find_prop(h1, "mac");
    m_mac2 = find_prop(h2, "mac");
    add_ip_pair(find_prop(h1, "ipv4"), find_prop(h2, "ipv4"));
    StrVect h1_v6 = find_ipv6s(h1);
    StrVect h2_v6 = find_ipv6s(h2);

    if(h1_v6.size() == h2_v6.size())
    {
        for(uint i=0; i<h1_v6.size(); ++i)
            add_ip_pair(h1_v6[i], h2_v6[i]);
    }
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

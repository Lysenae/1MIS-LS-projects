#ifndef HOSTGROUP_H
#define HOSTGROUP_H

#include "types.h"
#include "macaddr.h"
#include "ipv4addr.h"
#include "ipv6addr.h"

class HostGroup
{
public:
    HostGroup(StrVect h1, StrVect h2);
    std::string id();
    MACAddr *mac1();
    MACAddr *mac2();
    IPv4Addr *ipv4_1();
    IPv4Addr *ipv4_2();
    IPv6Addr *ipv6_1(uint idx);
    IPv6Addr *ipv6_2(uint idx);
    uint ipv6s_cnt();

    uint size();
    void print();

private:
    std::string  m_id;
    uint         m_pairs;
    IPv4Addr    *m_ip4_1;
    IPv4Addr    *m_ip4_2;
    IPv6Vect     m_ip6s_1;
    IPv6Vect     m_ip6s_2;
    MACAddr     *m_mac1;
    MACAddr     *m_mac2;

    void add_ip6_pair(std::string ip1, std::string ip2);
    StrVect find_ipv6s(StrVect inv);
    std::string find_prop(StrVect inv, std::string what);
    std::string find_mac(StrVect inv);
};

typedef std::vector<HostGroup *> HostGroups;

#endif // HOSTGROUP_H

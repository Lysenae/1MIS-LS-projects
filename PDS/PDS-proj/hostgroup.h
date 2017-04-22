#ifndef HOSTGROUP_H
#define HOSTGROUP_H

#include "types.h"

class HostGroup
{
public:
    HostGroup(std::string id, StrVect h1, StrVect h2);
    std::string id();
    StrVect ip_pair(uint idx);
    std::string mac(uint host);
    void add_ip_pair(std::string ip1, std::string ip2);
    uint size();
    void print();

private:
    std::string            m_id;
    uint                   m_pairs;
    StrVect                m_host1;
    StrVect                m_host2;
    std::string            m_mac1;
    std::string            m_mac2;

    StrVect find_ipv6s(StrVect inv);
    std::string find_prop(StrVect inv, std::string what);
    std::string find_mac(StrVect inv);
};

typedef std::vector<HostGroup *> HostGroups;

#endif // HOSTGROUP_H

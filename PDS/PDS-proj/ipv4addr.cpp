#include "ipv4addr.h"

IPv4Addr::IPv4Addr(ifaddrs *ifa) : IPAddr(ifa, IPVer::IPV4)
{
}

int IPv4Addr::mask_n()
{
    int mask = 0;
    for(uint i=0; i<OCTETS; ++i)
    {
        size_t ptr;
        std::string grp = mask_grp(i);
        uchar octet = (uchar)std::stoi(grp, &ptr);
        if(ptr == grp.size())
            mask += uchb(octet);
        else
        {
            mask = -1;
            break;
        }
    }
    return mask;
}

StrVect IPv4Addr::net_host_ips()
{
    StrVect v;
    int mask = mask_n();
    if(mask > 0)
    {
        int  octet = OCTETS - (mask / BITS +1);
        uint bits  = BITS - mask;
        bool whole = mask % BITS == 0;
        int  ov    = 0;
        int  max   = 0;

        v.push_back(addr_part(octet));
        max = maxval(whole ? BITS : bits % BITS);
        if(max < 0)
        {
            std::cerr << "Negative maxval result in net_host_ips\n";
            v.clear();
            return v;
        }

        if(!whole)
        {
            ov = get_addr_n(octet);
            if(ov < 0)
            {
                std::cerr << "Negative get_addr_n result in net_host_ips\n";
                v.clear();
                return v;
            }
        }


        v = expand_ips(v, max, ov, octet);
        v = remove_bc_net(v);
    }
    return v;
}

std::string IPv4Addr::get_group(std::string ins, uint idx)
{
    std::string s = "";
    if(idx < OCTETS)
    {
        StrVect grps = split_addr(ins, '.');
        s = grps[OCTETS - idx - 1];
    }
    return s;
}

int IPv4Addr::get_addr_n(uint idx)
{
    size_t ptr;
    std::string grp = addr_grp(idx);
    int octet = std::stoi(grp, &ptr);
    return ptr == grp.size() ? octet : -1;
}

int IPv4Addr::uchb(uchar uc)
{
    switch(uc)
    {
        case 255: return 8;
        case 254: return 7;
        case 252: return 6;
        case 248: return 5;
        case 240: return 4;
        case 224: return 3;
        case 192: return 2;
        case 128: return 1;
        case 0:   return 0;
        default:  return -1;
    }
}

int IPv4Addr::maxval(uint bits)
{
    switch(bits)
    {
        case 0:  return 0;
        case 1:  return 1;
        case 2:  return 3;
        case 3:  return 7;
        case 4:  return 15;
        case 5:  return 31;
        case 6:  return 63;
        case 7:  return 127;
        case 8:  return 255;
        default: return -1;
    }
}

std::string IPv4Addr::addr_part(uint until_octet)
{
    std::string addr = "";
    for(uint i=OCTETS-1; i>until_octet; --i)
    {
        addr += addr_grp(i);
        addr += ".";
    }
    return addr;
}

StrVect IPv4Addr::expand_ips(StrVect ips, int max, int base, int octet)
{
    if(octet < 0)
        return ips;

    std::string ipaddr;

    StrVect v;
    for(std::string ip : ips)
    {
        for(int i=0; i<=max; ++i)
        {
            ipaddr = std::string(ip + std::to_string(base + i) +
                ((octet == 0) ? "" : "."));
            if(ipaddr != m_addr)
                v.push_back(ipaddr);
        }
    }
    return expand_ips(v, UCHAR_MAX, 0, octet-1);
}

StrVect IPv4Addr::remove_bc_net(StrVect ips)
{
    StrVect v;
    for(uint i=0; i<ips.size(); ++i)
    {
        if(i == 0 || i == ips.size()-1)
            continue;
        v.push_back(ips[i]);
    }
    return v;
}

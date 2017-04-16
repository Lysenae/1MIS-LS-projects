#include "ipv6addr.h"

IPv6Addr::IPv6Addr(ifaddrs *ifa) : IPAddr(ifa, IPVer::IPV6)
{
}

std::string IPv6Addr::addr_grp(uint idx)
{
    return get_group(m_addr, idx);
}

std::string IPv6Addr::mask_grp(uint idx)
{
    return get_group(m_mask, idx);
}

UchrVect IPv6Addr::to_uchar()
{
    UchrVect uc;
    std::string s = "";
    std::cout << m_addr << std::endl;
    StrVect grps  = split_addr(m_addr, ':');
    uint nonempty = 0;
    uint empty    = 0;
    uint diff     = 0;
    StrVect newgrps;

    for(uint i=0; i<grps.size(); ++i)
    {
        if( grps[i] != "")
            ++nonempty;
        else
            empty = i;
    }
    for(uint i=0; i<grps.size(); ++i)
    {
        if(i == empty)
            for(uint j=0; j<(BLOCKS-nonempty); ++j)
                newgrps.push_back("0000");
        else if(grps[i] != "")
        {
            if(grps[i].size() < GRP_S)
            {
                diff = GRP_S - grps[i].size();
                s    = "";
                for(uint i=0; i<diff; ++i)
                    s += "0";
                s += grps[i];
            }
            else
                s = grps[i];
            newgrps.push_back(s);
        }
    }
    return uc;
}

std::string IPv6Addr::get_group(std::string ins, uint idx)
{
    std::string s = "";
    if(idx <= BLOCKS)
    {
        StrVect grps = split_addr(ins, ':');
        uint nonempty = 0;
        uint empty    = 0;
        for(uint i=0; i<grps.size(); ++i)
        {
            if( grps[i] != "")
                ++nonempty;
            else
                empty = i;
        }
        StrVect newgrps;
        for(uint i=0; i<grps.size(); ++i)
        {
            if(i == empty)
                for(uint j=0; j<(BLOCKS-nonempty); ++j)
                    newgrps.push_back("0");
            else if(grps[i] != "")
                newgrps.push_back(grps[i]);
        }
        s = newgrps[BLOCKS - idx - 1];
    }
    return s;
}

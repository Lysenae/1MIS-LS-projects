#include "ipv6addr.h"

IPv6Addr::IPv6Addr(ifaddrs *ifa) : IPAddr(ifa, IPVer::IPV6)
{
}

std::string IPv6Addr::get_group(std::string ins, uint idx)
{
    std::string s = "";
    if(idx <= IPV6_BLOCKS)
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
                for(uint j=0; j<(IPV6_BLOCKS-nonempty); ++j)
                    newgrps.push_back("0");
            else if(grps[i] != "")
                newgrps.push_back(grps[i]);
        }
        s = newgrps[IPV6_BLOCKS - idx - 1];
    }
    return s;
}

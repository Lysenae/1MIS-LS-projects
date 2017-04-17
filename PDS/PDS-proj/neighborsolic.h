#ifndef NEIGHBORSOLIC_H
#define NEIGHBORSOLIC_H

#include <netinet/ip6.h>

#include "ipv6addr.h"
#include "macaddr.h"
#include "types.h"

class NeighborSolic
{
public:
    static const uint LEN = 86;

    NeighborSolic(IPv6Addr *ip, MACAddr *mac);
    uchar *serialize();
    uint16_t checksum();

private:
    static const uint  ETH_HDR_LEN  = 14;
    static const uchar ICMPV6_NS_L  = 32;
    static const uchar ICMPV6_HDN   = 58;

    IPv6Addr *m_src_ip;
    MACAddr  *m_src_mac;
};

#endif // NEIGHBORSOLIC_H

#ifndef NEIGHBORSOLIC_H
#define NEIGHBORSOLIC_H

#include "ipv6addr.h"
#include "macaddr.h"
#include "types.h"

class NeighborSolic
{
public:
    static const uint LEN = 86;

    NeighborSolic(IPAddr *ip, MACAddr *mac);
    uchar *serialize();

private:
    IPv6Addr *m_ip;
    MACAddr  *m_mac;
};

#endif // NEIGHBORSOLIC_H

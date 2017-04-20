#ifndef NEIGHBORSOLIC_H
#define NEIGHBORSOLIC_H

#include <netinet/ip6.h>

#include "ipv6addr.h"
#include "macaddr.h"
#include "types.h"


/// \enum NDType
/// \brief definuje typ Neighbor Discovery paketu
/// \var NDType::NS
/// je Neighbor Solicitation paket
/// \var NDType::NA
/// je Neighbor Advertisiment paket
enum class NDType
{
    NS,
    NA
};

///
/// \brief Trieda NeighborDiscovery
///
class NeighborDiscovery
{
public:
    static const uint LEN = 86;

    NeighborDiscovery(NDType ndp, IPv6Addr *ip, MACAddr *mac);
    uchar *serialize();
    uint16_t checksum();

private:
    static const uint  ETH_HDR_LEN   = 14;
    static const uchar ICMPV6_NS_LEN = 32;
    static const uchar ICMPV6_TYPE   = 58;

    NDType    m_type;
    IPv6Addr *m_src_ip;
    MACAddr  *m_src_mac;
};

#endif // NEIGHBORSOLIC_H

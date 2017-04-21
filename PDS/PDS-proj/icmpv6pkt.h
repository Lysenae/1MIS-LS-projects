// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#ifndef ICMPV6PKT_H
#define ICMPV6PKT_H

#include <netinet/ip6.h>
#include <ctime>

#include "ipv6addr.h"
#include "packet.h"

/// \enum NDType
/// \brief definuje typ Neighbor Discovery paketu
/// \var IcmpV6Type::NS je Neighbor Solicitation paket
/// \var IcmpV6Type::NA je Neighbor Advertisiment paket
/// \var IcmpV6Type::Ping je Echo ping
enum class IcmpV6Type
{
    NS,
    NA,
    Ping,
};

///
/// \brief Trieda IcmpV6Pkt
///
class IcmpV6Pkt : public Packet
{
public:
    static const uint  IPV6_HDR_LEN     = 40;
    static const uchar ICMPV6_NS_LEN    = 32;
    static const uchar ICMPV6_NA_LEN    = 32;
    static const uchar ICMPV6_TYPE      = 58;
    static const uchar ICMPV6_NS_TYPE   = 0x87;
    static const uchar ICMPV6_NA_TYPE   = 0x88;
    static const uchar ICMPV6_PING_LEN  = 24;
    static const uchar ICMPV6_PING_TYPE = 0x80;

    IcmpV6Pkt(IcmpV6Type type, IPv6Addr *ip, MACAddr *mac);
    void set_dst_ip(IPv6Addr *ipv6);
    uint pktlen() const;
    uint payload_length() const;
    uchar *serialize(bool multicast = true);
    virtual sockaddr_ll sock_addr(int if_idx) override;

private:
    IcmpV6Type  m_type;
    IPv6Addr   *m_src_ip;
    IPv6Addr   *m_dst_ip;
    uchar       m_echo_id[2];

    uchar *ipv6_hdr();
    uchar *icmp_body();
    uchar *serialize_ns();
    uchar *serialize_na();
    uchar *serialize_echo();
    uint16_t checksum(uchar *icmp);
};

#endif // ICMPV6PKT_H

#ifndef PACKET_H
#define PACKET_H

#include <linux/if_ether.h>

#include "types.h"
#include "macaddr.h"

class Packet
{
public:
    Packet(MACAddr *src_mac);
    void set_dst_hwa(MACAddr *dst_mac);

protected:
    static const uint ETH_HDR_LEN = 14;
    const uint ETH_HW_TYPE        = 1;

    MACAddr *m_src_hwa;
    MACAddr *m_dst_hwa;
    uchar    m_src_hwa_o[MACAddr::OCTETS];
    uchar    m_dst_hwa_o[MACAddr::OCTETS];
    uint16_t m_eth_prot;

    enum class EthDest
    {
        UC,
        BC
    };

    uchar *eth_header(EthDest dest);
};

#endif // PACKET_H

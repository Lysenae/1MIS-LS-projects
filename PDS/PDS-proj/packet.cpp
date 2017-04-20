#include "packet.h"

Packet::Packet(MACAddr *src_mac)
{
    m_src_hwa = src_mac;
    m_dst_hwa = nullptr;

    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        m_src_hwa_o[i] = m_src_hwa->octet(i);
        m_dst_hwa_o[i] = 0x00;
    }
    m_eth_prot = 0;
}

void Packet::set_dst_hwa(MACAddr *dst_mac)
{
    m_dst_hwa = dst_mac;
    for(uint i=0; i<MACAddr::OCTETS; ++i)
        m_dst_hwa_o[i] = m_dst_hwa->octet(i);
}

uchar *Packet::eth_header(EthDest dest)
{
    uchar *hdr = new uchar[ETH_HDR_LEN];
    memset(hdr, 0, ETH_HDR_LEN);
    // Serialize Ethernet header
    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        if(dest == EthDest::BC)
            hdr[i] = 0xFF;           // Broadcast
        else
            hdr[i] = m_dst_hwa_o[i]; // Unicast
        hdr[MACAddr::OCTETS + i] = m_src_hwa_o[i];
    }
    memcpy(hdr+2*MACAddr::OCTETS, &m_eth_prot, S_USHORT);
    return hdr;
}

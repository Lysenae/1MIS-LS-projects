#include "arppkt.h"

ArpPkt::ArpPkt()
{
    m_hw_t      = 0;
    m_prot_t    = 0;
    m_hw_addl   = 0;
    m_prot_addl = 0;
    for(uint i=0; i<MACAddr::MAC_BLOCKS; ++i)
    {
        m_src_hwa[i] = 0;
        m_dst_hwa[i] = 0;
    }
    for(uint i=0; i<MACAddr::MAC_BLOCKS; ++i)
    {
        m_src_ip[i] = 0;
        m_dst_ip[i] = 0;
    }
}

void ArpPkt::set_hw_type(ushort hw_type)
{
    m_hw_t = hw_type;
}

void ArpPkt::set_protocol_type(ushort pt_type)
{
    m_prot_t = pt_type;
}

void ArpPkt::set_hw_add_ln(uchar hal)
{
    m_hw_addl = hal;
}

void ArpPkt::set_pt_add_ln(uchar pal)
{
    m_prot_addl = pal;
}

void ArpPkt::set_src_mac_addr(uint octet, uchar value)
{
    if(octet < MACAddr::MAC_BLOCKS)
        m_src_hwa[octet] = value;
}

void ArpPkt::set_src_ip_addr(uint octet, uchar value)
{
    if(octet < IPv4Addr::IPV4_BLOCKS)
        m_src_ip[octet] = value;
}

void ArpPkt::set_dst_mac_addr(uint octet, uchar value)
{
    if(octet < MACAddr::MAC_BLOCKS)
        m_dst_hwa[octet] = value;
}

void ArpPkt::set_dst_ip_addr(uint octet, uchar value)
{
    if(octet < IPv4Addr::IPV4_BLOCKS)
        m_dst_ip[octet] = value;
}

#ifndef ARPPKT_H
#define ARPPKT_H

#include "packet.h"

class ArpPkt : public Packet
{
public:
    ArpPkt();

private:
    ushort m_hw_t;
    ushort m_prot_t;
    uchar  m_hw_addl;
    uchar  m_prot_addl;
    ushort m_op;
    uchar  m_src_hwa[6];
    uchar  m_src_ip[4];
    uchar  m_dst_hwa[6];
    uchar  m_dst_ip[4];

    void set_hw_type(ushort hw_type);
    void set_protocol_type(ushort pt_type);
    void set_hw_add_ln(uchar hal);
    void set_pt_add_ln(uchar pal);
    void set_src_mac_addr(uint octet, uchar value);
    void set_src_ip_addr(uint octet, uchar value);
    void set_dst_mac_addr(uint octet, uchar value);
    void set_dst_ip_addr(uint octet, uchar value);

};

#endif // ARPPKT_H

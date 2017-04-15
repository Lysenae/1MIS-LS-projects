#ifndef ARPPKT_H
#define ARPPKT_H

#include <iostream>
#include <cstring>

#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>

#include "types.h"
#include "ipv4addr.h"
#include "macaddr.h"

enum class ArpPktType
{
    REQUEST,
    RESPONSE
};

class ArpPkt
{
public:
    ArpPkt(ArpPktType type, IPv4Addr *ip = nullptr, MACAddr *mac = nullptr);
    ~ArpPkt();
    void set_hw_type(ushort hw_type);
    void set_protocol_type(ushort pt_type);
    void set_hw_add_ln(uchar hal);
    void set_pt_add_ln(uchar pal);
    void set_src_mac_addr(uint octet, uchar value);
    void set_src_mac_addr(uint octet, std::string value);
    void set_src_ip_addr(uint octet, uchar value);
    void set_src_ip_addr(uint octet, std::string value);
    void set_dst_mac_addr(uint octet, uchar value);
    void set_dst_mac_addr(uint octet, std::string value);
    void set_dst_ip_addr(uint octet, uchar value);
    void set_dst_ip_addr(uint octet, std::string value);
    sockaddr_ll *sock_addr(int if_idx);
    uchar *serialize();
    void print();

private:
    const uint ETH_HW_TYPE = 1;
    const uint BUFF_SIZE   = 60;
    const uint ETH_HDR_LEN = 14;

    ArpPktType m_type;

    ushort m_hw_t;
    ushort m_prot_t;
    uchar  m_hw_addl;
    uchar  m_prot_addl;
    ushort m_op;
    uchar  m_src_hwa[6];
    uchar  m_src_ip[4];
    uchar  m_dst_hwa[6];
    uchar  m_dst_ip[4];

    sockaddr_ll *m_sock_addr;

    uchar str_to_uch(std::string s);
};

#endif // ARPPKT_H

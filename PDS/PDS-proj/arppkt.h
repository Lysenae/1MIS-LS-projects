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
    static const uint BUFF_LEN = 60;
    static const uint LEN      = 42;

    ArpPkt(ArpPktType type, IPv4Addr *ip = nullptr, MACAddr *mac = nullptr);
    void set_hw_type(ushort hw_type);
    void set_protocol_type(ushort pt_type);
    void set_hw_add_ln(uchar hal);
    void set_pt_add_ln(uchar pal);
    void set_src_mac_addr(uint octet, uchar value);
    void set_src_mac_addr(uint octet, std::string value);
    void set_src_ip_addr(uint octet, uchar value);
    void set_src_ip_addr(uint octet, std::string value);
    void set_src_ip_addr(IPv4Addr *ipv4);
    void set_dst_mac_addr(uint octet, uchar value);
    void set_dst_mac_addr(uint octet, std::string value);
    void set_dst_ip_addr(uint octet, uchar value);
    void set_dst_ip_addr(uint octet, std::string value);
    void set_dst_ip_addr(IPv4Addr *ipv4);
    sockaddr_ll sock_addr(int if_idx);
    uchar *serialize();
    void print();

    static MACAddr *parse_src_mac(uchar *pkt, int len);

private:
    static const uint ETH_HDR_LEN = 14;
    const uint ETH_HW_TYPE        = 1;
    const uint OC_ARP_REQ         = 0x01;
    const uint OC_ARP_RESP        = 0x02;

    enum class Field
    {
        HW_TYPE,
        PROT_TYPE,
        HW_ADDLN,
        PROT_ADDLN,
        OPCODE,
        SRC_HWA,
        SRC_IPA,
        DST_HWA,
        DST_IPA
    };

    ArpPktType m_type;

    uint16_t m_hw_t;
    uint16_t m_prot_t;
    uchar    m_hw_addl;
    uchar    m_prot_addl;
    uint16_t m_op;
    uchar    m_src_hwa[6];
    uchar    m_src_ip[4];
    uchar    m_dst_hwa[6];
    uchar    m_dst_ip[4];
    uint16_t m_eth_prot;

    uchar str_to_uch(std::string s);

    static uint offs(Field f, uint add_len);
};

#endif // ARPPKT_H

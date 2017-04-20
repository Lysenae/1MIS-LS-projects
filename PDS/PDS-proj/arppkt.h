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

class ArpPkt
{
public:
    static const uint BUFF_LEN = 60;
    static const uint LEN      = 42;

    ArpPkt(IPv4Addr *ip, MACAddr *mac);
    void set_src_ip_addr(uint octet, uchar value);
    void set_src_ip_addr(uint octet, std::string value);
    void set_src_ip_addr(IPv4Addr *ipv4);
    void set_dst_ip_addr(uint octet, uchar value);
    void set_dst_ip_addr(uint octet, std::string value);
    void set_dst_ip_addr(IPv4Addr *ipv4);
    sockaddr_ll sock_addr(int if_idx);
    uchar *serialize();
    MACAddr *parse_src_mac(uchar *pkt, int len, IPv4Addr **ip);

private:
    static const uint ETH_HDR_LEN = 14;
    const uint ETH_HW_TYPE        = 1;
    const uint OC_ARP_REQ         = 0x01;

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

    static uint offs(Field f, uint add_len);
};

#endif // ARPPKT_H

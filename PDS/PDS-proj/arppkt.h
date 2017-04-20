#ifndef ARPPKT_H
#define ARPPKT_H

#include <arpa/inet.h>
#include <linux/if_packet.h>

#include "packet.h"
#include "types.h"
#include "ipv4addr.h"
#include "macaddr.h"

class ArpPkt : public Packet
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
    const uint OC_ARP_REQ         = 0x01;

    enum class ArpField
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
    uchar    m_src_ip[4];
    uchar    m_dst_ip[4];

    static uint offs(ArpField f);
};

#endif // ARPPKT_H

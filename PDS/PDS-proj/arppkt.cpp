#include "arppkt.h"

ArpPkt::ArpPkt(IPv4Addr *ip, MACAddr *mac)
{
    m_hw_t      = htons(ETH_HW_TYPE);
    m_prot_t    = htons(ETH_P_IP);
    m_op        = htons(OC_ARP_REQ); // Request
    m_hw_addl   = (uchar) MACAddr::OCTETS;
    m_prot_addl = (uchar) IPv4Addr::OCTETS;
    m_eth_prot  = htons(ETH_P_ARP);

    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        m_src_hwa[i] = mac->octet(i);
        m_dst_hwa[i] = 0x00;
    }
    for(uint i=0; i<IPv4Addr::OCTETS; ++i)
        m_dst_ip[i] = 0;
    set_src_ip_addr(ip);
}

void ArpPkt::set_src_ip_addr(uint octet, uchar value)
{
    if(octet < IPv4Addr::OCTETS)
        m_src_ip[IPv4Addr::OCTETS - octet - 1] = value;
}

void ArpPkt::set_src_ip_addr(uint octet, std::string value)
{
    set_src_ip_addr(octet, str_to_uch(value));
}

void ArpPkt::set_src_ip_addr(IPv4Addr *ipv4)
{
    for(uint i=0; i<IPv4Addr::OCTETS; ++i)
    {
        if(ipv4 == nullptr)
            m_dst_ip[i] = 0;
        else
            set_src_ip_addr(i, ipv4->addr_grp(i));
    }
}

void ArpPkt::set_dst_ip_addr(uint octet, uchar value)
{
    if(octet < IPv4Addr::OCTETS)
        m_dst_ip[IPv4Addr::OCTETS - octet - 1] = value;
}

void ArpPkt::set_dst_ip_addr(uint octet, std::string value)
{
    set_dst_ip_addr(octet, str_to_uch(value));
}

void ArpPkt::set_dst_ip_addr(IPv4Addr *ipv4)
{
    for(uint i=0; i<IPv4Addr::OCTETS; ++i)
    {
        if(ipv4 == nullptr)
            m_dst_ip[i] = 0;
        else
            set_dst_ip_addr(i, ipv4->addr_grp(i));
    }
}

sockaddr_ll ArpPkt::sock_addr(int if_idx)
{
    sockaddr_ll sock_addr;
    sock_addr.sll_family   = AF_PACKET;
    sock_addr.sll_protocol = htons(ETH_P_ARP);
    sock_addr.sll_ifindex  = if_idx;
    sock_addr.sll_hatype   = htons(ETH_HW_TYPE);
    sock_addr.sll_pkttype  = (PACKET_BROADCAST);
    sock_addr.sll_halen    = MACAddr::OCTETS;
    sock_addr.sll_addr[6]  = 0x00;
    sock_addr.sll_addr[7]  = 0x00;
    for(uint i=0; i<MACAddr::OCTETS; ++i)
        sock_addr.sll_addr[i] = m_src_hwa[i];
    return sock_addr;
}

uchar *ArpPkt::serialize()
{
    uint o = ETH_HDR_LEN;
    uchar *buff = new uchar[BUFF_LEN];
    memset(buff, 0, BUFF_LEN);
    // Serialize Ethernet header
    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        buff[i]                   = 0xFF; // Broadcast
        buff[MACAddr::OCTETS + i] = m_src_hwa[i];
    }
    memcpy(buff+2*MACAddr::OCTETS, &m_eth_prot, S_USHORT);

    // Serialize ARP header
    memcpy(buff+offs(Field::HW_TYPE, o),    &m_hw_t,      S_USHORT); // HW type
    memcpy(buff+offs(Field::PROT_TYPE, o),  &m_prot_t,    S_USHORT); // Protocol type
    memcpy(buff+offs(Field::HW_ADDLN, o),   &m_hw_addl,   S_UCHAR);  // HW add length
    memcpy(buff+offs(Field::PROT_ADDLN, o), &m_prot_addl, S_UCHAR);  // Ptcl add ln
    memcpy(buff+offs(Field::OPCODE, o),     &m_op,        S_USHORT); // Opcode
    for(uint i=0; i<MACAddr::OCTETS; ++i) // Src/Dest HW addr
    {
        buff[offs(Field::SRC_HWA, o+i)] = m_src_hwa[i];
        buff[offs(Field::DST_HWA, o+i)] = m_dst_hwa[i];
    }
    for(uint i=0; i<IPv4Addr::OCTETS; ++i) // Src/Dest IP addr
    {
        buff[offs(Field::SRC_IPA, o+i)] = m_src_ip[i];
        buff[offs(Field::DST_IPA, o+i)] = m_dst_ip[i];
    }
    return buff;
}

MACAddr *ArpPkt::parse_src_mac(uchar *pkt, int len, IPv4Addr **ip)
{
    UchrVect mac;
    if(len > 0 && (uint)len >= offs(Field::SRC_HWA, ETH_HDR_LEN) + MACAddr::OCTETS)
    {
        std::string ips = "";
        uint16_t t;
        uchar p[2];
        p[0] = pkt[12];
        p[1] = pkt[13];
        bool valid_pkt = false;
        memcpy(&t, p, S_USHORT);
        if(htons(t) == ETH_P_ARP && pkt[21] == 0x02) // ARP Reply only
        {
            valid_pkt = true;
            for(uint i=0; i<MACAddr::OCTETS; ++i)
            {
                if(pkt[i] != m_src_hwa[i])
                {
                    valid_pkt = false;
                    break;
                }
            }

            if(valid_pkt)
            {
                for(uint i=0; i<MACAddr::OCTETS; ++i)
                    mac.push_back(pkt[offs(Field::SRC_HWA, ETH_HDR_LEN) + i]);
                for(uint i=0; i<IPv4Addr::OCTETS; ++i)
                {
                    ips += std::to_string(
                        (int)pkt[offs(Field::SRC_IPA, ETH_HDR_LEN) + i]);
                    if(i < IPv4Addr::OCTETS-1)
                        ips += ".";
                }
            }
            if(ips != "")
                *ip = new IPv4Addr(ips);
        }
    }
    return new MACAddr(mac);
}

uint ArpPkt::offs(Field f, uint add_len = 0)
{
    uint o = 0;
    switch(f)
    {
        case Field::HW_TYPE:    o = 0;  break;
        case Field::PROT_TYPE:  o = 2;  break;
        case Field::HW_ADDLN:   o = 4;  break;
        case Field::PROT_ADDLN: o = 5;  break;
        case Field::OPCODE:     o = 6;  break;
        case Field::SRC_HWA:    o = 8;  break;
        case Field::SRC_IPA:    o = 14; break;
        case Field::DST_HWA:    o = 18; break;
        case Field::DST_IPA:    o = 24; break;
        default: break;
    }
    return o + add_len;
}

#include "neighbordiscovery.h"

NeighborDiscovery::NeighborDiscovery(NDType ndp, IPv6Addr *ip, MACAddr *mac) :
Packet(mac)
{
    m_type       = ndp;
    m_src_ip     = ip;
    m_dst_ip     = nullptr;
    m_eth_prot   = htons(ETH_P_IPV6);
    m_echo_id[0] = 0x00;
    m_echo_id[1] = 0x00;
    srand(time(nullptr));
}

void NeighborDiscovery::set_dst_ip(IPv6Addr *ipv6)
{
    m_dst_ip = ipv6;
}

uint NeighborDiscovery::pktlen() const
{
    return ETH_HDR_LEN + IPV6_HDR_LEN + payload_length();
}

uint NeighborDiscovery::payload_length() const
{
    switch(m_type)
    {
        case NDType::NS:    return ICMPV6_NS_LEN;
        case NDType::NA:    return ICMPV6_NA_LEN;
        case NDType::EchoP: return ECHOP_LEN;
        default:            return 0;
    }
}

uchar *NeighborDiscovery::serialize()
{
    uchar *buff    = new uchar[pktlen()];
    uchar *ehdr    = eth_header(EthDest::BCv6);
    uchar *ihdr    = ipv6_hdr();
    uchar *icmphdr = serialize_echo();
    memset(buff, 0, pktlen());
    for(uint i=0; i<ETH_HDR_LEN; ++i)
        buff[i] = ehdr[i];
    for(uint i=0; i<IPV6_HDR_LEN; ++i)
        buff[i+ETH_HDR_LEN] = ihdr[i];
    for(uint i=0; i<payload_length(); ++i)
        buff[i+ETH_HDR_LEN+IPV6_HDR_LEN] = icmphdr[i];
    return buff;
}

sockaddr_ll NeighborDiscovery::sock_addr(int if_idx)
{
    sockaddr_ll sock_addr;
    sock_addr.sll_family   = AF_PACKET;
    sock_addr.sll_protocol = htons(ETH_P_IPV6);
    sock_addr.sll_ifindex  = if_idx;
    sock_addr.sll_hatype   = htons(ETH_HW_TYPE);
    sock_addr.sll_pkttype  = (PACKET_BROADCAST);
    sock_addr.sll_halen    = MACAddr::OCTETS;
    sock_addr.sll_addr[6]  = 0x00;
    sock_addr.sll_addr[7]  = 0x00;
    for(uint i=0; i<MACAddr::OCTETS; ++i)
        sock_addr.sll_addr[i] = m_src_hwa_o[i];
    return sock_addr;
}

uchar *NeighborDiscovery::ipv6_hdr()
{
    uchar *hdr        = new uchar[IPV6_HDR_LEN];
    uint16_t pl       = htons(payload_length());
    UchrVect src_ip_u = m_src_ip->to_uchar();
    UchrVect dst_ip_u = m_dst_ip->to_uchar();

    memset(hdr, 0, IPV6_HDR_LEN);
    hdr[0] = 0x60;                // Version
    // Traffic class + Flow label [1-3] boli nastavene v memset, vynechane
    memcpy(hdr+4, &pl, S_USHORT); // Payload length
    hdr[6] = ICMPV6_TYPE;         // Next header
    hdr[7] = 0xFF;                // Hop limit, pre ND povinne 255
    for(uint i=0; i<IPv6Addr::BYTES; ++i)
    {
        hdr[8 + i]                   = src_ip_u[i];
        hdr[8 + i + IPv6Addr::BYTES] = dst_ip_u[i];
    }
    return hdr;
}

uchar *NeighborDiscovery::serialize_ns()
{
    uchar *hdr        = new uchar[payload_length()];
    UchrVect dst_ip_u = m_dst_ip->to_uchar();
    memset(hdr, 0, payload_length());
    hdr[0] = ICMPV6_NS_TYPE; // Type 135 (0x87)
    // Code, Checksum, Reserved nastavene v memset, byty 1-7
    for(uint i=0; i<dst_ip_u.size(); ++i) // Target IP addr
        hdr[8+i] = dst_ip_u[i];
    hdr[24] = 0x01;                       // Opts type
    hdr[25] = 0x01;                       // Opts length
    for(uint i=0; i<MACAddr::OCTETS; ++i) // Source MAC
        hdr[26+i] = m_src_hwa_o[i];
    uint16_t chks = checksum(hdr);
    memcpy(hdr+2, &chks, S_USHORT);
    return hdr;
}

uchar *NeighborDiscovery::serialize_na()
{
    uchar *hdr = new uchar[payload_length()];
    memset(hdr, 0, payload_length());
    return hdr;
}

uchar *NeighborDiscovery::serialize_echo()
{
    uchar *hdr        = new uchar[payload_length()];
    UchrVect src_ip_u = m_src_ip->to_uchar();
    memset(hdr, 0, payload_length());
    hdr[0] = ECHOP_TYPE;                  // Type 128 (0x80)
    // Code v memset, checksum [1-3]
    m_echo_id[0] = rand() % 255;          // Identifier
    m_echo_id[1] = rand() % 255;
    hdr[4] = m_echo_id[0];
    hdr[5] = m_echo_id[1];
    hdr[7] = 0x01;                        // Sequence
    for(uint i=0; i<src_ip_u.size(); ++i) // Source IP addr ako data
        hdr[8+i] = src_ip_u[IPv6Addr::BYTES -1 - i];
    uint16_t chks = checksum(hdr);
    memcpy(hdr+2, &chks, S_USHORT);
    return hdr;
}

uint16_t NeighborDiscovery::checksum(uchar *icmp)
{
    uint16_t tmp16;
    uchar    tmp[2];
    UchrVect src_ip_u = m_src_ip->to_uchar();
    UchrVect dst_ip_u = m_dst_ip->to_uchar();
    uint32_t sum = 0;
    uint16_t checksum;
    char buffer[20];

    // IPv6 Pseudo-header
    for(uint i=0; i<src_ip_u.size(); i+=2)
    {
        tmp[0] = src_ip_u[i];
        tmp[1] = src_ip_u[i+1];
        memcpy(&tmp16, tmp, S_USHORT);
        sum += tmp16;
    }

    for(uint i=0; i<dst_ip_u.size(); i+=2)
    {
        tmp[0] = dst_ip_u[i];
        tmp[1] = dst_ip_u[i+1];
        memcpy(&tmp16, tmp, S_USHORT);
        sum += tmp16;
    }
    sum += htons(payload_length());
    sum += htons(ICMPV6_TYPE);

    // ICMPv6 Body
    for(uint i=0; i<payload_length(); i += 2)
    {
        tmp[0] = icmp[i];
        tmp[1] = icmp[i+1];
        memcpy(&tmp16, tmp, S_USHORT);
        sum += tmp16;
    }

    sum += sum >> 16;
    checksum = (uint16_t)~sum;
    uchar t[2];
    memcpy(t, &checksum, 2);
    sprintf(buffer, "%02X %02X", t[0],t[1]);
    std::cout << "CH: " << buffer << std::endl;
    return checksum;
}

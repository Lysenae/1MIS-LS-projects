#include "arppkt.h"

ArpPkt::ArpPkt(ArpPktType type, IPv4Addr *ip, MACAddr *mac)
{
    m_type      = type;
    m_hw_t      = 0;
    m_prot_t    = 0;
    m_hw_addl   = 0;
    m_prot_addl = 0;
    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        if(mac == nullptr)
            m_src_hwa[i] = 0;
        else
            m_src_hwa[i] = mac->octet(i);

        if(m_type == ArpPktType::REQUEST)
            m_dst_hwa[i] = 0xff;
        else
            m_dst_hwa[i] = 0;
    }
    for(uint i=0; i<IPv4Addr::OCTETS; ++i)
    {
        if(ip == nullptr)
            m_src_ip[i] = 0;
        else
            set_src_ip_addr(i, ip->addr_grp(i));
        m_dst_ip[i] = 0;
    }

    m_sock_addr = new sockaddr_ll;
}

ArpPkt::~ArpPkt()
{
    delete m_sock_addr;
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
    if(octet < MACAddr::OCTETS)
        m_src_hwa[octet] = value;
}

void ArpPkt::set_src_mac_addr(uint octet, std::string value)
{
    set_src_mac_addr(octet, str_to_uch(value));
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

void ArpPkt::set_dst_mac_addr(uint octet, uchar value)
{
    if(octet < MACAddr::OCTETS)
        m_dst_hwa[octet] = value;
}

void ArpPkt::set_dst_mac_addr(uint octet, std::string value)
{
    set_dst_mac_addr(octet, str_to_uch(value));
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

sockaddr_ll *ArpPkt::sock_addr(int if_idx)
{
    if(m_src_hwa[0] != (uchar)0)
    {
        m_sock_addr->sll_family   = AF_PACKET;
        m_sock_addr->sll_protocol = htons(ETH_P_ARP);
        m_sock_addr->sll_ifindex  = if_idx;
        m_sock_addr->sll_hatype   = htons(ETH_HW_TYPE);
        m_sock_addr->sll_pkttype  = (PACKET_BROADCAST);
        m_sock_addr->sll_halen    = MACAddr::OCTETS;
        m_sock_addr->sll_addr[6]  = 0x00;
        m_sock_addr->sll_addr[7]  = 0x00;
        for(uint i=0; i<MACAddr::OCTETS; ++i)
            m_sock_addr->sll_addr[i] = m_src_hwa[i];
        return m_sock_addr;
    }
    std::cerr << "ArpPkt::sock_addr(" << if_idx << "): Source MAC not set" << std::endl;
    return nullptr;
}

uchar *ArpPkt::serialize()
{
    uchar *buff = new uchar[BUFF_SIZE];
    memset(buff, 0, BUFF_SIZE);
    // Serialize Ethernet header
    for(uint i=0; i<MACAddr::OCTETS; ++i)
    {
        memcpy(buff+i, &m_dst_hwa[i], S_UCHAR);
        memcpy(buff+i+MACAddr::OCTETS, &m_src_hwa[i], S_UCHAR);
    }
    uint16_t prot = htons(ETH_P_ARP);
    memcpy(buff+2*MACAddr::OCTETS, &prot, S_USHORT);
    return buff;
}

void ArpPkt::print()
{
    char buffer[20];
    sprintf(buffer, "%02X:%02X:%02X:%02X:%02X:%02X",
        m_src_hwa[0],m_src_hwa[1],m_src_hwa[2],m_src_hwa[3],m_src_hwa[4],m_src_hwa[5]);
    std::cout << "Src MAC: " << std::string(buffer) << std::endl;
    std::cout << "Src IP: ";
    for(uint i = 0; i<IPv4Addr::OCTETS; ++i)
        std::cout << (int)m_src_ip[i] << (i==IPv4Addr::OCTETS-1 ? "" : ".");
    std::cout << std::endl;
}

uchar ArpPkt::str_to_uch(std::string s)
{
    size_t ptr;
    int val = std::stoi(s, &ptr);
    if(val >= (int)UCHAR_MIN && val <= (int)UCHAR_MAX && ptr == s.size())
        return (uchar) val;
    std::cerr << "ArpPkt::str_to_uch(" << s << "): Conversion error" << std::endl;
    return (uchar) 0;
}

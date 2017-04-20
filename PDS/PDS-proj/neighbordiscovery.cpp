#include "neighbordiscovery.h"

NeighborDiscovery::NeighborDiscovery(NDType ndp, IPv6Addr *ip, MACAddr *mac)
{
    m_type    = ndp;
    m_src_ip  = ip;
    m_src_mac = mac;
    checksum();
}

uchar *NeighborDiscovery::serialize()
{
    //uchar *buff = new uchar[LEN];
    // Create Ethernet Header
    return 0x00;
}

uint16_t NeighborDiscovery::checksum()
{
    IPv6Addr *src_ip = new IPv6Addr("fe80::3305:34c7:8b78:6f6d", "FFFF:FFFF:FFFF:FFFF::");
    IPv6Addr *dst_ip = new IPv6Addr("fe80::bf3c:dbad:db55:f6e1", "FFFF:FFFF:FFFF:FFFF::");
    uchar icmp[ICMPV6_NS_LEN] = {
        0x87,
        0x00,
        0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0xFE, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xBF, 0x3C, 0xDB, 0xAD, 0xDB, 0x55, 0xF6, 0xE1,
        0x01,
        0x01,
        0xDC, 0x4A, 0x3E, 0xDB, 0x43, 0x99
    };

    uint16_t tmp16;
    uchar    tmp[2];
    UchrVect src_ip_u = src_ip->to_uchar();
    UchrVect dst_ip_u = dst_ip->to_uchar();
    uint32_t sum = 0;
    uint16_t checksum;
    char buffer[20];

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
    sum += htons(ICMPV6_NS_LEN);
    sum += htons(ICMPV6_TYPE);

    for(uint i=0; i<ICMPV6_NS_LEN; i += 2)
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
    return checksum;
}

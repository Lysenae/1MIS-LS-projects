#include "neighborsolic.h"

NeighborSolic::NeighborSolic(IPAddr *ip, MACAddr *mac)
{
    m_ip  = ip;
    m_mac = mac;
}

uchar *NeighborSolic::serialize()
{
    uchar *buff = new uchar[LEN];
    // Create Ethernet Header

}

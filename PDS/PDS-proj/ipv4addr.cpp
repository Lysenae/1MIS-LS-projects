#include "ipv4addr.h"

IPv4Addr::IPv4Addr(ifaddrs *ifa) : NetAddr(ifa, IPVer::IPV4)
{
}

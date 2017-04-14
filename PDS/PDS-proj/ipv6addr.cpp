#include "ipv6addr.h"

IPv6Addr::IPv6Addr(ifaddrs *ifa) : NetAddr(ifa, IPVer::IPV6)
{
}

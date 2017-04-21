// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#ifndef MACADDR_H
#define MACADDR_H

#include <cstdio>

#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

#include "types.h"
#include "socket.h"

///
/// \brief MAC adresa
///
class MACAddr
{
public:
    static const uint OCTETS = 6;

    MACAddr(ifreq *ifr);
    MACAddr(UchrVect oct);
    MACAddr(std::string mac);
    std::string to_string() const;
    uchar octet(uint idx);
    bool eq(MACAddr *other);
    bool empty() const;

private:
    uchar m_mac[OCTETS];
};

#endif // MACADDR_H

#include <iostream>
#include <cstdio>
#include <cstdlib>

#include "netitf.h"
#include "arppkt.h"
#include "socket.h"

using namespace std;

int main(int argc, char *argv[])
{
    int opt;
    std::string fname     = "";
    std::string interface = "";
    bool opts_ok          = true;

    while ((opt = getopt(argc, argv, "i:f:")) != -1)
    {
        switch (opt)
        {
            case 'i':
                interface = std::string(optarg);
                break;
            case 'f':
                fname = std::string(optarg);
                break;
            default:
                opts_ok = false;
                break;
        }
    }

    if(interface.empty() || fname.empty() || !opts_ok)
    {
        fprintf(stderr, "Usage: %s -i interface -f file\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    NetItf      *netitf    = new NetItf(interface);
    MACAddr     *loc_mac   = netitf->mac();
    IPv4Addr    *loc_ipv4  = netitf->ipv4();
    IPv6Vect     loc_ipv6s = netitf->ipv6();
    //StrVect      v4s       = loc_ipv4->net_host_ips();
    //IPv4Addr    *v4another = nullptr;
    ArpPkt      *apkt      = new ArpPkt(loc_ipv4, loc_mac);
    //sockaddr_ll  saddr     = apkt->sock_addr(netitf->index());
    //MACAddr    *tm         = nullptr;
    //uchar buf[ArpPkt::BUFF_LEN];

    Socket s(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if(s.open() != SocketStatus::OPENED)
    {
        cerr << "pds-scanner: Failed to open socket" << endl;
        return -1;
    }

    /*for(std::string ip : v4s)
    {
        memset(buf, 0, ArpPkt::BUFF_LEN);
        v4another = new IPv4Addr(ip, loc_ipv4->snmask());
        apkt->set_dst_ip_addr(v4another);
        s.send_to(apkt->serialize(), ArpPkt::LEN, 0, (sockaddr*)&saddr,
            sizeof(saddr));
        int rcvd = s.recv_from(buf, ArpPkt::BUFF_LEN-1, 0, nullptr, nullptr);
        tm = ArpPkt::parse_src_mac(buf, rcvd);
        if(!tm->eq(loc_mac) && !tm->empty())
            cout << v4another->addr() << " has MAC " << tm->to_string() << endl;

        delete tm;
        tm = nullptr;
        delete v4another;
        v4another = nullptr;
    }*/

    char buff[2];
    for(IPv6Addr *v6 : loc_ipv6s)
    {
        cout << v6->addr() << "/" << v6->snmask() << endl;
        UchrVect v = v6->to_uchar();
        for(uchar u : v)
        {
            sprintf(buff, "%02X", u);
            cout << string(buff) << endl;
        }
    }

    delete netitf;
    delete loc_ipv4;
    delete loc_mac;
    delete apkt;
    return 0;
}

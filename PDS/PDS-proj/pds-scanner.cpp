#include <csignal>

#include "types.h"
#include "hash.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "neighbordiscovery.h"

using namespace std;

bool search = true;

void on_sigint(int signum)
{
    cout << " SIGINT(" << signum << ") detected" << endl;
    search = false;
}


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

    signal(SIGINT, on_sigint);

    NetItf      *netitf    = new NetItf(interface);
    MACAddr     *loc_mac   = netitf->mac();
    IPv4Addr    *loc_ipv4  = netitf->ipv4();
    IPv6Vect     loc_ipv6s = netitf->ipv6();
    StrVect      v4s       = loc_ipv4->net_host_ips();
    IPv4Addr    *v4another = nullptr;
    ArpPkt      *apkt      = new ArpPkt(loc_ipv4, loc_mac);
    sockaddr_ll  saddr_v4  = apkt->sock_addr(netitf->index());
    MACAddr    *tm         = nullptr;
    uchar buf[ArpPkt::BUFF_LEN];

    Socket s4(PF_PACKET, SOCK_RAW, htons(ETH_P_ARP));
    if(s4.open() != SocketStatus::OPENED)
    {
        cerr << "pds-scanner: Failed to open socket for ARP packets" << endl;
        return OP_FAIL;
    }

    int rcvd;
    cout << "Searching for IPv4 hosts" << endl;
    for(std::string ip : v4s)
    {
        if(!search)
            break;
        memset(buf, 0, ArpPkt::BUFF_LEN);
        v4another = new IPv4Addr(ip, loc_ipv4->snmask());
        apkt->set_dst_ip_addr(v4another);
        s4.send_to(apkt->serialize(), ArpPkt::LEN, 0, (sockaddr*)&saddr_v4,
            sizeof(saddr_v4));

        rcvd = s4.recv_from(buf, ArpPkt::BUFF_LEN-1, 0, nullptr, nullptr);
        IPv4Addr *ipa  = nullptr;
        tm = apkt->parse_src_mac(buf, rcvd, &ipa);
        if(!tm->eq(loc_mac) && !tm->empty() && ipa != nullptr)
            cout << ipa->addr() << " has MAC " << tm->to_string() << endl;

        delete tm;
        tm = nullptr;
        delete v4another;
        v4another = nullptr;
    }
    cout << "Searching for IPv4 hosts copmleted" << endl;
    s4.close();
    search = true;

    Socket s6(PF_PACKET, SOCK_RAW, htons(ETH_P_IPV6));
    if(s6.open() != SocketStatus::OPENED)
    {
        cerr << "pds-scanner: Failed to open socket for ARP packets" << endl;
        return OP_FAIL;
    }

    NeighborDiscovery *ns  = new NeighborDiscovery(NDType::EchoP, loc_ipv6s[2], loc_mac);
    sockaddr_ll  saddr_v6  = ns->sock_addr(netitf->index());
    ns->set_dst_ip(new IPv6Addr("ff02::1"));
    uchar *nsu = ns->serialize();
    cout << "Sending solicit from:" << loc_ipv6s[2]->addr() << endl;
    s6.send_to(nsu, ns->pktlen(), 0, (sockaddr*)&saddr_v6, sizeof(saddr_v6));
    uchar *buf_v6 = new uchar[500];
    uint16_t pl;
    uchar plb[2];
    uint cnt = 0;
    while(true)
    {
        if(!search)
            break;
        if(cnt == 25)
            break; // Pocet neecho ping reply paketov za sebou prekrocil hranicu
        rcvd = s6.recv_from(buf_v6, 500, 0, nullptr, nullptr);
        plb[0] = buf_v6[18];
        plb[1] = buf_v6[19];
        memcpy(&pl, plb, S_USHORT);
        pl = ntohs(pl);
        if(pl == 24)
        {
            if(buf_v6[54] == 0x81)
            {
                cnt = 0;
                continue;
            }
        }
        else if(pl == 32)
        {
            if(buf_v6[54] == 0x87)
            {
                cnt = 0;
                continue;
            }
        }
        cnt++;
    }

    delete netitf;
    delete loc_ipv4;
    delete loc_mac;
    delete apkt;
    return 0;
}

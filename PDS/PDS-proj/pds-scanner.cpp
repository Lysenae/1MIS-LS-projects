// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include <csignal>

#include "types.h"
#include "hash.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "icmpv6pkt.h"

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

    Hash h;
    NetItf      *netitf    = new NetItf(interface);
    MACAddr     *loc_mac   = netitf->mac();
    IPv4Addr    *loc_ipv4  = netitf->ipv4();
    IPv6Vect     loc_ipv6s = netitf->ipv6();
    StrVect      v4s       = loc_ipv4->net_host_ips();
    IPv4Addr    *v4another = nullptr;
    ArpPkt      *apkt      = new ArpPkt(ArpType::Request, loc_ipv4, loc_mac);
    sockaddr_ll  saddr_v4  = apkt->sock_addr(netitf->index());
    MACAddr    *tm         = nullptr;
    uchar buf[ArpPkt::BUFF_LEN];

    for(IPv6Addr *a : loc_ipv6s)
    {
        std::cout << "IPV6: " << a->addr() << endl;
    }

    Socket s4(PF_PACKET, SOCK_RAW, htons(ETH_P_ARP));
    if(s4.open() != SocketStatus::Opened)
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
        if(apkt->analyze_pkt(buf, rcvd, &tm, &ipa))
        {
            if(!tm->eq(loc_mac) && !tm->empty() && ipa->addr() == ip)
            {
                cout << ipa->addr() << " has MAC " << tm->to_string() << endl;
                h.add_value(tm->to_string(), ipa->addr());
            }
        }

        delete tm;
        tm = nullptr;
        delete v4another;
        v4another = nullptr;
    }
    cout << "Searching for IPv4 hosts copmleted" << endl;
    h.print();
    s4.close();
    search = true; // TODO Remove

    Socket s6(PF_PACKET, SOCK_RAW, htons(ETH_P_IPV6));
    if(s6.open() != SocketStatus::Opened)
    {
        cerr << "pds-scanner: Failed to open socket for ARP packets" << endl;
        return OP_FAIL;
    }

    IcmpV6Pkt *nde  = new IcmpV6Pkt(IcmpV6Type::Ping, loc_ipv6s[2], loc_mac);
    sockaddr_ll  saddr_v6  = nde->sock_addr(netitf->index());
    nde->set_dst_ip(new IPv6Addr("ff02::1"));
    uchar *nsu = nde->serialize();
    cout << "Sending solicit from:" << loc_ipv6s[2]->addr() << endl;
    //s6.send_to(nsu, nde->pktlen(), 0, (sockaddr*)&saddr_v6, sizeof(saddr_v6));
    uchar *buf_v6   = new uchar[500];
    uint cnt        = 0;
    uint keys       = h.keys().size();
    std::string mac = "";
    std::string ip6 = "";
    int pl;
    while(true)
    {
        break;
        if(!search || keys == 0 || cnt == 50)
            break;
        rcvd = s6.recv_from(buf_v6, 500, 0, nullptr, nullptr);
        pl   = (int)buf_v6[19];
        if(pl == 24 || pl == 32) // Mozno echo reply alebo ns
        {
            if(buf_v6[54] == 0x81 || buf_v6[54] == 0x87)
            {
                mac = "";
                ip6 = "";
                for(uint i=6; i<12; ++i)
                {
                    mac += str_bytes8(buf_v6[i]);
                    mac += (i == 11 ? "" : ":");
                }
                if(h.has_key(mac))
                {
                    for(uint i=22; i<38; ++i)
                    {
                        ip6 += str_bytes8(buf_v6[i]);
                        ip6 += (i < 37 && i % 2 == 1) ? ":" : "";
                    }
                    h.add_existing(mac, ip6);
                    keys--;
                    if(keys == 0)
                        break;
                }
                cnt = 0;
                continue;
            }
        }
        cnt++;
    }

    h.print();

    IcmpV6Pkt *nds  = new IcmpV6Pkt(IcmpV6Type::NS, loc_ipv6s[1], loc_mac);
    nds->set_dst_ip(new IPv6Addr("::"));
    StrVect macs = h.keys();
    nsu = nds->serialize(false);
    for(std::string m : macs)
    {
        cout << "NDS: '" << m << "'" << endl;
        nds->set_dst_hwa(new MACAddr(m));
        s6.send_to(nsu, nde->pktlen(), 0, (sockaddr*)&saddr_v6, sizeof(saddr_v6));
    }
    h.print();
    s6.close();

    delete nds;
    delete nde;
    delete netitf;
    delete loc_ipv4;
    delete loc_mac;
    delete apkt;
    return 0;
}

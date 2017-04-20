#include <csignal>

#include "types.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "neighborsolic.h"

using namespace std;

bool search = true;

void on_sigint(int signum)
{
    cout << "SIGINT(" << signum << ") detected" << endl;
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
    sockaddr_ll  saddr     = apkt->sock_addr(netitf->index());
    MACAddr    *tm         = nullptr;
    uchar buf[ArpPkt::BUFF_LEN];

    Socket s(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if(s.open() != SocketStatus::OPENED)
    {
        cerr << "pds-scanner: Failed to open socket" << endl;
        return -1;
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
        s.send_to(apkt->serialize(), ArpPkt::LEN, 0, (sockaddr*)&saddr,
            sizeof(saddr));

        rcvd = s.recv_from(buf, ArpPkt::BUFF_LEN-1, 0, nullptr, nullptr);
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

    NeighborSolic *ns = new NeighborSolic(loc_ipv6s[0], loc_mac);
    uint16_t checksum = ns->checksum();
    cout << "Checksum: " << checksum << " (" << str_bytes16(checksum) << ")" << endl;

    delete netitf;
    delete loc_ipv4;
    delete loc_mac;
    delete apkt;
    return 0;
}

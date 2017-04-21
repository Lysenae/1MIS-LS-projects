// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include "types.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "icmpv6pkt.h"

using namespace std;

///
/// \brief Priznak urcujuci, ci sa bude este pokracovat v zasielani paketov,
/// SIGINT ho nastavi na false
///
bool do_spoofing = true;

void print_usage();
bool spoof_arp();
bool spoof_ndp();

int main(int argc, char **argv)
{
    bool all_ok = true;
    StrVect args;
    for(int i=0; i<argc; ++i)
        args.push_back(string(argv[i]));

    string interface   = "";
    string interval    = "";
    string protocol    = "";
    string v1ip        = "";
    string v1mac       = "";
    string v2ip        = "";
    string v2mac       = "";
    uint   tm_interval = 0;

    if(args.size() != 15)
    {
        print_usage();
        return OP_FAIL;
    }

    for(uint i=1; i<args.size(); i += 2) // Preskoc nazov binarky
    {
        if(args[i][0] == '-')
        {
            if     (args[i] == "-i")          interface = args[i+1];
            else if(args[i] == "-t")          interval  = args[i+1];
            else if(args[i] == "-p")          protocol  = args[i+1];
            else if(args[i] == "-victim1ip")  v1ip      = args[i+1];
            else if(args[i] == "-victim1mac") v1mac     = args[i+1];
            else if(args[i] == "-victim2ip")  v2ip      = args[i+1];
            else if(args[i] == "-victim2mac") v2mac     = args[i+1];
            else
            {
                cerr << "Unknown parameter " << args[i] << endl;
                print_usage();
                return OP_FAIL;
            }
        }
        else
        {
            print_usage();
            return OP_FAIL;
        }
    }

    if(protocol != "arp" && protocol != "ndp")
    {
        cerr << "Invalid protocol '" << protocol << "'" << endl;
        all_ok = false;
    }

    size_t ptr;
    tm_interval = stoi(interval, &ptr);
    if(ptr != interval.size())
    {
        cerr << "Invalid interval value '" << interval << "'" << endl;
        all_ok = false;
    }

    NetItf *netitf = new NetItf(interface);
    if(netitf->index() < 0)
    {
        cerr << "Failed to find netinterface '" << interface << "'" << endl;
        all_ok = false;
    }

    IPVer v1ipv = IPAddr::get_version(v1ip);
    IPVer v2ipv = IPAddr::get_version(v2ip);
    if(v1ipv == IPVer::Undef || v2ipv == IPVer::Undef)
    {
        cerr << "Invalid IP address format" << endl;
        all_ok = false;
    }
    if(v1ipv != v2ipv)
    {
        cerr << "Different IP address versions" << endl;
        all_ok = false;
    }

    IPv6Addr *v1ip6  = nullptr;
    IPv6Addr *v2ip6  = nullptr;
    IPv4Addr *v1ip4  = nullptr;
    IPv4Addr *v2ip4  = nullptr;
    MACAddr  *v1maca = nullptr;
    MACAddr  *v2maca = nullptr;

    if(v1ipv == IPVer::IPv4)
    {
        v1ip4 = new IPv4Addr(v1ip);
        v2ip4 = new IPv4Addr(v2ip);
        if(v1ip4->empty())
        {
            cerr << "Invalid IPv4 address: " << v1ip << endl;
            all_ok = false;
        }
        if(v2ip4->empty())
        {
            cerr << "Invalid IPv4 address: " << v2ip << endl;
            all_ok = false;
        }
    }
    else
    {
        v1ip6 = new IPv6Addr(v1ip);
        v2ip6 = new IPv6Addr(v2ip);
        if(v1ip6->empty())
        {
            cerr << "Invalid IPv4 address: " << v1ip << endl;
            all_ok = false;
        }
        if(v2ip6->empty())
        {
            cerr << "Invalid IPv4 address: " << v2ip << endl;
            all_ok = false;
        }
    }

    v1maca = new MACAddr(v1mac);
    v2maca = new MACAddr(v2mac);
    if(v1maca->empty())
    {
        cerr << "Invalid MAC address: " << v1mac << endl;
        all_ok = false;
    }
    if(v2maca->empty())
    {
        cerr << "Invalid MAC address: " << v2mac << endl;
        all_ok = false;
    }

    cout << "Interface: " << interface << endl;
    cout << "Interval:  " << interval << " - " << tm_interval << endl;
    cout << "Protocol:  " << protocol << endl;
    cout << "V1 IP:     " << v1ip << endl;
    cout << "V1 MAC:    " << v1mac << endl;
    cout << "V2 IP:     " << v2ip << endl;
    cout << "v2 MAC:    " << v2mac << endl;

    if(all_ok)
    {
        if(protocol == "arp")
        {
            spoof_arp();
        }
        else
        {
            spoof_ndp();
        }
    }

    if(v1ip6 != nullptr) delete v1ip6;
    if(v2ip6 != nullptr) delete v2ip6;
    if(v1ip4 != nullptr) delete v1ip4;
    if(v2ip4 != nullptr) delete v2ip4;
    delete v1maca;
    delete v2maca;
    delete netitf;
    return 0;
}

void print_usage()
{
    cout << "Usage: " << "pds-spoof -i interface -t sec -p protocol " <<
        "-victim1ip ipaddress -victim1mac macaddress -victim2ip " <<
        "ipaddress -victim2mac macaddress" << endl;
}

bool spoof_arp()
{
    return false;
}

bool spoof_ndp()
{
    return false;
}

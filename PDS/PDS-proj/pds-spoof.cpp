// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include "types.h"
#include "hash.h"
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

int main(int argc, char **argv)
{
    //./pds-spoof -i interface -t sec -p protocol -victim1ip ipaddress -victim1mac macaddress -victim2ip ipaddress -victim2mac macaddress
    StrVect args;
    for(int i=0; i<argc; ++i)
        args.push_back(string(argv[i]));

    string interface = "";
    string interval  = "";
    string protocol  = "";
    string v1ip      = "";
    string v1mac     = "";
    string v2ip      = "";
    string v2mac     = "";

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
    cout << "Interface: " << interface << endl;
    cout << "Interval:  " << interval << endl;
    cout << "Protocol:  " << protocol << endl;
    cout << "V1 IP:     " << v1ip << endl;
    cout << "V1 MAC:    " << v1mac << endl;
    cout << "V2 IP:     " << v2ip << endl;
    cout << "v2 MAC:    " << v2mac << endl;
    return 0;
}


void print_usage()
{
    cout << "Usage: " << "pds-spoof -i interface -t sec -p protocol " <<
        "-victim1ip ipaddress -victim1mac macaddress -victim2ip " <<
        "ipaddress -victim2mac macaddress" << endl;
}

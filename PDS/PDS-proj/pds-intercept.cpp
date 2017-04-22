// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include <csignal>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

#include "types.h"
#include "hash.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "icmpv6pkt.h"

using namespace std;

///
/// \brief Priznak urcujuci, ci sa bude este pokracovat v zachytavani paketov,
/// SIGINT ho nastavi na false
///
bool do_intercept = true;

void print_usage();
void on_sigint(int signum);

int main(int argc, char **argv)
{
    int opt;
    string fname     = "";
    string interface = "";
    bool opts_ok     = true;
    bool all_ok      = true;

    while ((opt = getopt(argc, argv, "i:f:")) != -1)
    {
        switch (opt)
        {
            case 'i':
                interface = string(optarg);
                break;
            case 'f':
                fname = string(optarg);
                break;
            default:
                opts_ok = false;
                break;
        }
    }

    if(interface.empty() || fname.empty() || !opts_ok)
    {
        print_usage();
        return OP_FAIL;
    }

    cout << "interface: " << interface << endl;
    cout << "fname:     " << fname << endl;

    signal(SIGINT, on_sigint);

    return all_ok ? OP_SUCC : OP_FAIL;
}

///
/// \brief Vypise pouzitie programu
///
void print_usage()
{
    cout << "Usage: pds-intercept -i interface -f hosts_xml" << endl;
}

///
/// \brief SIGINT handler
/// \param signum
///
void on_sigint(int signum)
{
    cout << " SIGINT(" << signum << ") detected" << endl;
    do_intercept = false;
}

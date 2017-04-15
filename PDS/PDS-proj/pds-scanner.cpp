#include <iostream>
#include <cstdlib>

#include <unistd.h>

#include "net.h"

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

    Net net(interface);
    cout << "Interface index: " << net.if_index() << endl;
    IPv4Addr *v4 = net.ipv4();
    if(v4)
    {
        cout << "IPv4 [" << v4->interface() << "] " << v4->addr() << "/" <<
            v4->mask_n() << endl;
        cout << "MASK: " << v4->snmask() << endl;
        /*StrVect ips = v4->net_host_ips();
        for(uint i=0; i<ips.size(); ++i)
            cout << ips[i] << endl;*/
    }
    IPv6Vect v6s = net.ipv6();
    for(uint i=0; i<v6s.size(); ++i)
    {
        cout << "IPv6 [" << v6s[i]->interface() << "] " << v6s[i]->addr() << "/" <<
            v6s[i]->snmask() << endl;
    }
    return 0;
}

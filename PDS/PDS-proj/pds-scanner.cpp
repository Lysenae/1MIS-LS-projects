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

    std::cout << "Interface: " << interface << ", file: " << fname << std::endl;

    Net net;
    IPv4Addr *v4 = net.ipv4(interface);
    if(v4)
        cout << "IPv4 [" << v4->interface() << "] " << v4->addr() << "/" <<
            v4->snmask() << std::endl;
    IPv6Vect v6s = net.ipv6(interface);
    for(uint i=0; i<v6s.size(); ++i)
        cout << "IPv6 [" << v6s[i]->interface() << "] " << v6s[i]->addr() << "/" <<
            v6s[i]->snmask() << std::endl;

    return 0;
}

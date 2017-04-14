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
    std::string ipv6 = net.ipv6(interface).size() > 0 ? net.ipv6(interface)[0] : "";
    cout << "IPv4 [" << interface << "] " << net.ipv4(interface) << std::endl;
    cout << "IPv6 [" << interface << "] " << ipv6 << std::endl;

    return 0;
}

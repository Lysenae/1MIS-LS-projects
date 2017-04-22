// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include <csignal>
#include <libxml/xmlreader.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

#include "types.h"
#include "hash.h"
#include "netitf.h"
#include "arppkt.h"
#include "socket.h"
#include "icmpv6pkt.h"
#include "hostgroup.h"

using namespace std;

///
/// \brief Priznak urcujuci, ci sa bude este pokracovat v zachytavani paketov,
/// SIGINT ho nastavi na false
///
bool do_intercept = true;

void print_usage();
void on_sigint(int signum);
bool get_groups(std::string fname, HostGroups *hgs);
void parse_elements(xmlNode * node, vector<StrVect *> *sv);
bool intercept(HostGroups *hgs);

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

    HostGroups hgs;
    all_ok = get_groups(fname, &hgs);

    if(all_ok)
    {
        if(!intercept(&hgs))
        {
            cerr << "Intercept failed" << endl;
            all_ok = false;
        }
    }

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

///
/// \brief Ziska skupiny zo zadaneho XML. Ciastocne prebrane z
/// http://www.xmlsoft.org/examples/tree1.c
/// \param fname cesta ku XML suboru
/// \param hg skupiny
/// \return false pri chybe
///
bool get_groups(std::string fname, HostGroups *hgs)
{
    (void) hgs;

    xmlDoc *doc           = nullptr;
    xmlNode *root_element = nullptr;

    vector<StrVect *> v;

    doc = xmlReadFile(fname.c_str(), nullptr, 0);

    if (doc == nullptr)
    {
        cerr << "Failed to open XML file" << endl;
        return false;
    }

    root_element = xmlDocGetRootElement(doc);
    parse_elements(root_element, &v);

    for(StrVect *sv : v)
    {
        for(string s : *sv)
        {
            cout << s << endl;
        }
        cout << endl;
    }

    xmlFreeDoc(doc);

    xmlCleanupParser();

    return true;
}

void parse_elements(xmlNode * node, vector<StrVect *> *sv)
{
    xmlNode *cur_node = nullptr;
    xmlAttrPtr attr;
    xmlChar *attr_v;
    string nname = "";
    bool to_add  = true;


    for (cur_node = node; cur_node; cur_node = cur_node->next)
    {
        if (cur_node->type == XML_ELEMENT_NODE)
        {
            nname = string((char*)cur_node->name);
            if(nname == "host")
            {
                to_add = false;
                for(attr=cur_node->properties; attr!=nullptr; attr=attr->next)
                {
                    if(string((char*)attr->name) == "group")
                    {
                        to_add = true;
                    }
                }
                if(to_add)
                {
                    sv->push_back(new StrVect());
                    for(attr=cur_node->properties; attr!=nullptr; attr=attr->next)
                    {
                        attr_v = xmlGetProp(cur_node, attr->name);
                        sv->at(sv->size()-1)->push_back(string(string(
                            (char*)attr->name)) + ":" + string((char*)attr_v));
                    }
                }
            }
            else if(nname == "ipv4")
            {
                sv->at(sv->size()-1)->push_back(string("ipv4:" +
                    string((char*)cur_node->children->content)));
            }
            else if(nname == "ipv6")
            {
                sv->at(sv->size()-1)->push_back(string("ipv6:" +
                    string((char*)cur_node->children->content)));
            }
        }

        if(to_add)
            parse_elements(cur_node->children, sv);
    }
}

bool intercept(HostGroups *hgs)
{
    (void) hgs;
    return false;
}

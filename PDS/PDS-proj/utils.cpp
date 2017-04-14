#include "utils.h"

std::vector<std::string> Utils::split(std::string s, char delimiter)
{
    std::vector<std::string> v;
    std::string t = "";
    for(uint i=0; i<s.size(); ++i)
    {
        if(s[i] == delimiter)
        {
            v.push_back(t);
            t = "";
        }
        else
        {
            t += s[i];
            if(i == s.size()-1)
                v.push_back(t);
        }
    }
    return v;
}

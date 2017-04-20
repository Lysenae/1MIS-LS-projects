#include "hash.h"

Hash::Hash()
{

}

void Hash::add_existing(std::string key, std::string value)
{
    if(has_key(key))
    {
        if(!has_value(key, value))
            add_value(key, value);
    }
}

StrVect Hash::keys()
{
    StrVect v;
    for(std::map<std::string, StrVect*>::const_iterator it = m_map.begin(); it != m_map.end(); ++it)
      v.push_back(it->first);
    return v;
}

bool Hash::has_key(std::string key)
{
    StrVect ks = keys();
    for(std::string k : ks)
    {
        if(k == key)
            return true;
    }
    return false;
}

bool Hash::has_value(std::string key, std::string value)
{
    if(!has_key(key))
        return false;
    StrVect *vals = m_map[key];
    for(std::string v : *vals)
    {
        if(v == value)
            return true;
    }
    return false;
}

void Hash::add_value(std::string key, std::string value)
{
    if(!has_key(key))
    {
        m_map[key] = new StrVect();
    }
    m_map[key]->push_back(value);
}

void Hash::print()
{
    StrVect ks = keys();
    for(std::string k : ks)
    {
        std::cout << k << std::endl;
        StrVect *vals = m_map[k];
        for(std::string v : *vals)
            std::cout << "\t" << v << std::endl;
    }
}

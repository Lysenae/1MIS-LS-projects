#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>

#include "utype.h"

class Utils
{
public:
    static std::vector<std::string> split(std::string s, char delimiter);
};

#endif // UTILS_H

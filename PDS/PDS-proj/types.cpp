#include "types.h"

std::string str_bytes16(uint16_t t)
{
    uchar u[2];
    char buffer[5];
    memcpy(u, &t, S_USHORT);
    sprintf(buffer, "%02X %02X", u[0], u[1]);
    return std::string(buffer);
}

uchar str_to_uch(std::string s)
{
    size_t ptr;
    int val = std::stoi(s, &ptr);
    if(val >= (int)UCHAR_MIN && val <= (int)UCHAR_MAX && ptr == s.size())
        return (uchar) val;
    std::cerr << "str_to_uch(" << s << "): Conversion error" << std::endl;
    return (uchar) 0;
}

std::string str_bytes8(uchar t)
{
    char buffer[2];
    sprintf(buffer, "%02X", t);
    return std::string(buffer);
}

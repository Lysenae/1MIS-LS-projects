#include "types.h"

std::string str_bytes16(uint16_t t)
{
    uchar u[2];
    char buffer[2];
    memcpy(u, &t, S_USHORT);
    sprintf(buffer, "%02X %02X", u[0], u[1]);
    return std::string(buffer);
}

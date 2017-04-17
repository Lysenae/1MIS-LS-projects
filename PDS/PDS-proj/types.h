#ifndef TYPES_H
#define TYPES_H

#include <string>
#include <vector>
#include <iostream>
#include <cstdio>
#include <cstring>
#include <cstdlib>

typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned int   uint;

typedef std::vector<std::string> StrVect;
typedef std::vector<uchar>       UchrVect;

const uint BITS       = 8;
const uint UCHAR_MIN  = 0;
const uint UCHAR_MAX  = 255;
const int  OP_FAIL    = -1;
const uint S_UCHAR    = 1;
const uint S_USHORT   = 2;

std::string str_bytes16(uint16_t t);

#endif // TYPES_H

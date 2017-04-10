/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   utils.h
 * ************************************************************************** */

#ifndef UTILS_H
#define UTILS_H

#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *str_dup(const char *s);
char *str_trim(const char *s);

#endif // UTILS_H

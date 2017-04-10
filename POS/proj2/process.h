/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   process.h
 * ************************************************************************** */

#ifndef PROCESS_H
#define PROCESS_H

#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "utils.h"
#include "str_vector.h"

struct Process
{
  char *in_file;
  char *out_file;
  bool background;
  struct StrVector params;
};

void p_init(struct Process *p, char *args);
void p_destroy(struct Process *p);


#endif // PROCESS_H

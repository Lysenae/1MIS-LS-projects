/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   vector.h
 * ************************************************************************** */

#ifndef VECTOR_H
#define VECTOR_H

#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

struct Vector
{
  char **vals;
  int length;
};

void v_init(struct Vector *v);
void v_destroy(struct Vector *v);
void v_append(struct Vector *v, const char *s);
int v_size(struct Vector *v);
char *v_at(struct Vector *v, int idx);
void v_print(struct Vector *v);

#endif // VECTOR_H

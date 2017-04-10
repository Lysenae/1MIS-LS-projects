/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   vector.c
 * ************************************************************************** */

#include "vector.h"

void v_init(struct Vector *v)
{
  v->vals   = NULL;
  v->length = 0;
}

void v_destroy(struct Vector *v)
{
  for(int i=0; i<v_size(v); ++i)
  {
    free(v->vals[i]);
    v->vals[i] = NULL;
  }
  free(v->vals);
  v_init(v);
}

void v_append(struct Vector *v, const char* s)
{
  size_t len = strlen(s);
  v->vals = (char**)realloc(v->vals, sizeof(char *)*(v->length+1));
  v->vals[v->length] = (char *)malloc(sizeof(char)*(len+1));
  strcpy(v->vals[v->length], s);
  v->vals[v->length][len] = '\0';
  v->length += 1;
}

int v_size(struct Vector *v)
{
  return v->length;
}

char *v_at(struct Vector *v, int idx)
{
  if(idx >= 0 && idx < v_size(v))
  {
    size_t len = strlen(v->vals[idx]);
    char *s = (char *)malloc(sizeof(char)*(len+1));
    strcpy(s, v->vals[idx]);
    s[len] = '\0';
    return s;
  }
  return NULL;
}

void v_print(struct Vector *v)
{
  for(int i=0; i<v_size(v); ++i)
    printf("%s\n", v_at(v, i));
}

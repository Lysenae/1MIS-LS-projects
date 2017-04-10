/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   utils.c
 * ************************************************************************** */

#include "utils.h"

/** Odstrani biele znaky na zaciatku a konci retazca. Prebrane z
 * http://stackoverflow.com/questions/122616/how-do-i-trim-leading-trailing-
 * whitespace-in-a-standard-way.
 * @param s retazec
 * @return ocisteny retazec
**/
char *trim(char *s)
{
  char * p = s;
  int l = strlen(p);
  while(isspace(p[l - 1])) p[--l] = 0;
  while(* p && isspace(* p)) ++p, --l;
  memmove(s, p, l + 1);
  return s;
}

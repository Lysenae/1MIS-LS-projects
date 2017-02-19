/* ************************************************************************** *
 * Projekt: POS #1 - Ticket algoritmus
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-02-19
 * Subor:   ticket.c
 * ************************************************************************** */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int getticket();
void await(int aenter);
void advance();

int main(int argc, char **argv)
{
  for(int i=0; i<argc; ++i)
  {
    printf("%s\n", argv[i]);
  }

  return 0;
}

int getticket()
{
  return 0;
}

void await(int aenter)
{
  printf("%d\n", aenter);
}

void advance()
{
}

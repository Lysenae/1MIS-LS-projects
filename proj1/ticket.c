/* ************************************************************************** *
 * Projekt: POS #1 - Ticket algoritmus
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-02-19
 * Subor:   ticket.c
 * ************************************************************************** */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <pthread.h>

int strtoi(char *str);
int getticket();
void await(int aenter);
void advance();
void *print_id(void *t);

int main(int argc, char **argv)
{
  if(argc != 3)
  {
    printf("./ticket nthreads ncpath\n");
    return EXIT_FAILURE;
  }

  int nthreads = strtoi(argv[1]);
  int ncpath   = strtoi(argv[2]);

  if(nthreads < 0 || ncpath < 0)
  {
    fprintf(stderr, "Neplatny typ alebo hodnota parametrov\n");
    return EXIT_FAILURE;
  }

  pthread_t threads[nthreads];
  long tids[nthreads];

  int res;
  for(long id=0; id<=nthreads; ++id)
  {
    tids[id] = id;
    res = pthread_create(&threads[id], NULL, print_id, (void *)id);
    if(res)
    {
      fprintf(stderr, "Nepodarilo sa vytvorit vlakno %ld\n", id);
      return EXIT_FAILURE;
    }
  }

  printf("tids[0] = %ld\n", tids[0]);
  pthread_exit(NULL);
  return EXIT_SUCCESS;
}

/**
 * @brief Prevedie retazec na kladne cele cislo.
 * @param str vstupny retazec
 * @return prevedene kladne cislo alebo -1 v pripade chyby
 */
int strtoi(char *str)
{
  char *ptr;
  long num = strtol(str, &ptr, 10);
  if(strcmp("", ptr) == 0 && num <= INT_MAX && num >= 0)
    return (int) num;
  else
    return -1;
}

int getticket()
{
  static int ticket = -1;
  ticket += 1;
  return ticket;
}

void await(int aenter)
{
  printf("%d\n", aenter);
}

void advance()
{
}

void *print_id(void *t)
{
  printf("Thread: %ld\n", (long)t);
  pthread_exit(NULL);
}

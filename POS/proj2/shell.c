/* ************************************************************************** *
 * Projekt: POS #1 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-09
 * Subor:   shell.c
 * ************************************************************************** */

#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>

void *th_read_rt(void *t);
void *th_run_rt(void *t);

int main()
{
  pthread_t th_read, th_run;

  int data = 1;
  pthread_create(&th_read, NULL, th_read_rt, (void *) &data);
  pthread_create(&th_run, NULL, th_run_rt, (void *) &data);

  pthread_join(th_read, NULL);
  pthread_join(th_run, NULL);

  return 0;
}

void *th_read_rt(void *t)
{
  int d = *((int *) t);
  printf("read %d\n", d);
  pthread_exit(NULL);

}

void *th_run_rt(void *t)
{
  int d = *((int *) t);
  printf("run %d\n", d);
  pthread_exit(NULL);
}

/* ************************************************************************** *
 * Projekt: POS #2 - Shell
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

#include "monitor.h"

void *th_read_rt(void *t);
void *th_run_rt(void *t);

TMonitor monitor;

int main()
{
  if(!mt_init(&monitor))
    return -1;

  pthread_t th_read, th_run;

  pthread_create(&th_read, NULL, th_read_rt, (void *) &monitor);
  pthread_create(&th_run, NULL, th_run_rt, (void *) &monitor);

  pthread_join(th_read, NULL);
  pthread_join(th_run, NULL);

  return 0;
}

void *th_read_rt(void *t)
{
  TMonitor *m = (TMonitor *) t;
  printf("read %s\n", mt_running(m) ? "true" : "false");
  mt_shutdown(m);
  pthread_exit(NULL);

}

void *th_run_rt(void *t)
{
  TMonitor *m = (TMonitor *) t;
  printf("run %s\n", mt_running(m) ? "true" : "false");
  pthread_exit(NULL);
}

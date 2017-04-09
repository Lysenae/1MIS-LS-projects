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

#define STDIN  0
#define STDOUT 1

#define RUN  100
#define READ 101

void *th_rt_read(void *t);
void *th_rt_run(void *t);

TMonitor monitor;

int main()
{
  if(!mt_init(&monitor))
    return -1;

  pthread_t th_read, th_run;

  pthread_create(&th_read, NULL, th_rt_read, (void *) &monitor);
  pthread_create(&th_run, NULL, th_rt_run, (void *) &monitor);

  pthread_join(th_read, NULL);
  pthread_join(th_run, NULL);

  return 0;
}

void *th_rt_read(void *t)
{
  TMonitor *m = (TMonitor *) t;
  int rc;
  char buff[MT_CMDLEN+1];

  while(mt_running(m))
  {
    printf("$ ");
    fflush(stdout);
    memset(buff, 0, MT_CMDLEN);
    rc = read(STDIN, buff, MT_CMDLEN);
    if(rc == MT_CMDLEN)
    {
      fprintf(stderr, "Command too long\n");
      while(getchar() != '\n') {} // precitaj prikaz az do konca
      buff[0] = '\0';
    }
    buff[rc-1] = '\0';

    if(strcmp(buff, "exit") == 0)
    {
      mt_shutdown(m);
      mt_signal(m, READ);
      break;
    }
    else if(strcmp(buff, "") == 0)
    {
      continue;
    }
    else
    {
      mt_set_cmd(m, buff);
      mt_signal(m, READ);
      mt_wait(m, READ);
    }
  }
  pthread_exit(NULL);
}

void *th_rt_run(void *t)
{
  TMonitor *m = (TMonitor *) t;

  while(mt_running(m))
  {
    mt_wait(m, RUN);

    if(!mt_running(m))
      break;

    printf("Run: %s\n", mt_get_cmd(m));
    mt_signal(m, RUN);
  }
  pthread_exit(NULL);
}

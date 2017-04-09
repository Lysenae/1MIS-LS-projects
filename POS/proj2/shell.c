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

#define CMDLEN 512

typedef struct t_monitor
{
  bool            running;
  char            command[CMDLEN+1];
  pthread_mutex_t mtx_th;
  pthread_mutex_t mtx_proc;
  pthread_cond_t  cond;
} TMonitor;

void mt_init(TMonitor *m);
void mt_shutdown(TMonitor *m);
bool mt_running(TMonitor *m);

void *th_read_rt(void *t);
void *th_run_rt(void *t);

TMonitor monitor;

int main()
{
  mt_init(&monitor);

  pthread_t th_read, th_run;

  pthread_create(&th_read, NULL, th_read_rt, (void *) &monitor);
  pthread_create(&th_run, NULL, th_run_rt, (void *) &monitor);

  pthread_join(th_read, NULL);
  pthread_join(th_run, NULL);

  return 0;
}

void mt_init(TMonitor *m)
{
  m->running = true;
  memset(&m->command, 0, CMDLEN+1);
}

void mt_shutdown(TMonitor *m)
{
  pthread_mutex_lock(&m->mtx_th);
  m->running = false;
  pthread_mutex_unlock(&m->mtx_th);
}

bool mt_running(TMonitor *m)
{
  bool rslt;
  pthread_mutex_lock(&m->mtx_th);
  rslt = m->running;
  pthread_mutex_unlock(&m->mtx_th);
  return rslt;
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

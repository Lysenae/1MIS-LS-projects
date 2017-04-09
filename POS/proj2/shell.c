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

bool mt_init(TMonitor *m);
void mt_shutdown(TMonitor *m);
bool mt_running(TMonitor *m);

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

bool mt_init(TMonitor *m)
{
  int rc = 0;
  m->running = true;
  memset(&m->command, 0, CMDLEN+1);
  rc += pthread_mutex_init(&m->mtx_th, NULL);
  rc += pthread_mutex_init(&m->mtx_proc, NULL);
  rc += pthread_cond_init(&m->cond, NULL);
  if(rc != 0)
  {
    fprintf(stderr, "Failed to initilize monitor\n");
    return false;
  }
  return true;
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

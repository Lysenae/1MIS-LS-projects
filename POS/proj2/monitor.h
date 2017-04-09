/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-09
 * Subor:   monitor.h
 * ************************************************************************** */

#ifndef MONITOR_H
#define MONITOR_H

#define _XOPEN_SOURCE
#define _XOPEN_SOURCE_EXTENDED 1

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>

#define MT_CMDLEN 513

typedef struct t_monitor
{
  bool            running;
  char            command[MT_CMDLEN];
  int             cond_id;
  pthread_mutex_t mtx_th;
  pthread_mutex_t mtx_data;
  pthread_mutex_t mtx_proc;
  pthread_cond_t  cond;
} TMonitor;

bool mt_init(TMonitor *m);
void mt_shutdown(TMonitor *m);
bool mt_running(TMonitor *m);
void mt_wait(TMonitor *m, int id);
void mt_signal(TMonitor *m, int id);
void mt_set_cmd(TMonitor *m, const char *cmd);
char *mt_get_cmd(TMonitor *m);

#endif // MONITOR_H

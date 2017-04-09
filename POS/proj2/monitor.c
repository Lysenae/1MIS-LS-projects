/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-09
 * Subor:   monitor.c
 * ************************************************************************** */

#include "monitor.h"

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

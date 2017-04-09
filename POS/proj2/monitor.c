/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-09
 * Subor:   monitor.c
 * ************************************************************************** */

#include "monitor.h"

static void s_mt_set_cond_id(TMonitor *m, int id)
{
  pthread_mutex_lock(&m->mtx_data);
  m->cond_id = id;
  pthread_mutex_unlock(&m->mtx_data);
}

static int s_mt_get_cond_id(TMonitor *m)
{
  int id;
  pthread_mutex_lock(&m->mtx_data);
  id = m->cond_id;
  pthread_mutex_unlock(&m->mtx_data);
  return id;
}

bool mt_init(TMonitor *m)
{
  int rc = 0;
  m->running = true;
  m->cond_id = 0;
  memset(&m->command, 0, MT_CMDLEN);
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
  pthread_mutex_lock(&m->mtx_data);
  m->running = false;
  pthread_mutex_unlock(&m->mtx_data);
}

bool mt_running(TMonitor *m)
{
  bool rslt;
  pthread_mutex_lock(&m->mtx_data);
  rslt = m->running;
  pthread_mutex_unlock(&m->mtx_data);
  return rslt;
}

void mt_wait(TMonitor *m, int id)
{
  s_mt_set_cond_id(m, id);
  pthread_mutex_lock(&m->mtx_th);
  while(id == s_mt_get_cond_id(m))
    pthread_cond_wait(&m->cond, &m->mtx_th);
  pthread_mutex_unlock(&m->mtx_th);
}

void mt_signal(TMonitor *m, int id)
{
  s_mt_set_cond_id(m, id);
  pthread_mutex_lock(&m->mtx_th);
  pthread_cond_signal(&m->cond);
  pthread_mutex_unlock(&m->mtx_th);
}

void mt_set_cmd(TMonitor *m, const char *cmd)
{
  pthread_mutex_lock(&m->mtx_data);
  strcpy(m->command, cmd);
  pthread_mutex_unlock(&m->mtx_data);
}

char *mt_get_cmd(TMonitor *m)
{
  char *buff = malloc(sizeof(m->command));
  pthread_mutex_lock(&m->mtx_data);
  strcpy(buff, m->command);
  pthread_mutex_unlock(&m->mtx_data);
  return buff;
}

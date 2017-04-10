/* ************************************************************************** *
 * Projekt: POS #2 - Shell
 * Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz
 * Datum:   2017-04-10
 * Subor:   process.c
 * ************************************************************************** */

#include "process.h"

static void s_p_reset(struct Process *p)
{
  p->in_file    = NULL;
  p->out_file   = NULL;
  p->background = false;
}

static void s_p_init(struct Process *p)
{
  s_p_reset(p);
  v_init(&p->params);
}

void p_init(struct Process *p, char *args)
{
  s_p_init(p);
  printf("Init process with args: '%s'\n", args);
}

void p_destroy(struct Process *p)
{
  s_p_reset(p);
  v_destroy(&p->params);
}

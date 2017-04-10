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
  p->args       = NULL;
  p->background = false;
}

static void s_p_init(struct Process *p)
{
  s_p_reset(p);
  v_init(&p->params);
}

void p_init(struct Process *p, const char *args)
{
  s_p_init(p);
  p_set_args(p, args);
  p_set_background(p);
  printf("Init process with args: '%s'\n", args);
}

void p_destroy(struct Process *p)
{
  if(p->args != NULL)     free(p->args);
  if(p->in_file != NULL)  free(p->in_file);
  if(p->out_file != NULL) free(p->out_file);
  s_p_reset(p);
  v_destroy(&p->params);
}

void p_set_args(struct Process *p, const char *args)
{
  printf("Setting args '%s'\n", args);
  if(p->args != NULL)
  {
    free(p->args);
    p->args = NULL;
  }
  size_t len = strlen(args);
  p->args = (char *)calloc(sizeof(char), (len)+1);
  strcpy(p->args, args);
}

void p_set_background(struct Process *p)
{
  char *s = str_dup(p->args);
  size_t len = strlen(s);
  size_t idx = 0;
  for(size_t i = len-1; i>0; --i)
  {
    if(s[i] == ' ')
      continue;
    else
    {
      if(s[i] == '&')
      {
        p->background = true;
        idx = i;
      }
      break;
    }
  }
  if(idx > 0)
  {
    for(size_t i=idx; i<len; ++i)
      s[i] = '\0';
    p_set_args(p, str_trim(s));
  }
}

void p_print(struct Process *p)
{
  printf("Process:\n  In: '%s'\n  Out: '%s'\n  Args: '%s'\n  Bckg: %s\n",
    p->in_file != NULL ? p->in_file : "N/A",
    p->out_file != NULL ? p->out_file : "N/A",
    p->args != NULL ? p->args : "N/A",
    p->background ? "true" : "false"
  );
}

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
  p->invalid    = false;
}

static void s_p_init(struct Process *p)
{
  s_p_reset(p);
  v_init(&p->params);
}

static char s_p_rcpl(char r)
{
  return r == '<' ? '>' : '<';
}

void p_init(struct Process *p, const char *args)
{
  s_p_init(p);
  p_set_args(p, args);
  p_set_background(p);
  p_set_file(p, '<');
  p_set_file(p, '>');
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
  if(p->args != NULL)
  {
    free(p->args);
    p->args = NULL;
  }
  str_set(&p->args, args);
}

void p_set_background(struct Process *p)
{
  char *s = str_dup(p->args);
  size_t len = strlen(s);
  size_t idx = 0;
  if(str_chrn(s, '&') > 1)
  {
    p->invalid = true;
  }
  else
  {
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
}

void p_set_file(struct Process *p, char c)
{
  if(c != '<' && c != '>')
    return;

  char *s = str_dup(p->args);
  char f[MAX_FNAME];
  memset(f, 0, MAX_FNAME);
  int len = strlen(s);
  int occ = str_chrn(s, c);
  int idx1, idx2;
  int fi = 0;
  if(occ > 1)
  {
    p->invalid = true;
  }
  else if(occ == 1)
  {
    idx1 = str_fstc(s, c);
    idx2 = idx1 + 1;
    while(s[idx2] == ' ' && idx2 < len) ++idx2;
    while(s[idx2] != ' ' && idx2 < len)
    {
      f[fi] = s[idx2];
      ++idx2;
      ++fi;
    }
    while(s[idx2] == ' ' && idx2 < len) ++idx2;
    if(idx2 < len && s[idx2] != s_p_rcpl(c))
    {
      p->invalid = true;
    }
    else
    {
      str_deln(s, idx1, idx2-idx1);
      p_set_args(p, str_trim(s));
      if(strlen(f) < 1)
        p->invalid = true;
      else
      {
        if(c == '<')
          str_set(&p->in_file, f);
        else
          str_set(&p->out_file, f);
      }
    }
  }
}

void p_print(struct Process *p)
{
  printf("Process:\n  In: '%s'\n  Out: '%s'\n  Args: '%s'\n  Bckg: %s\n  Inv: %s\n",
    p->in_file != NULL ? p->in_file : "N/A",
    p->out_file != NULL ? p->out_file : "N/A",
    p->args != NULL ? p->args : "N/A",
    p->background ? "true" : "false",
    p->invalid ? "true" : "false"
  );
}

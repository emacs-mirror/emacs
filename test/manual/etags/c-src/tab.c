/*
** tab.c for  in
**
** Made by Pierric
** Login   <pierric@seignobosc.com>
**
** Started on  Thu Jan 24 18:36:47 2002 Pierric
** Last update Mon Sep 23 18:02:02 2002 Pierric
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "my_malloc.h"

static int		count_words(char *str, char delim)
{
  int                   count;

  count = 0;
  while (*str)
    {
      if (*str != delim)
	{
	  count++;
	  if (!strchr(str + 1, delim))
	    return (count);
	  str = strchr(str + 1, delim);
	}
      else
	str++;
    }
  return (count);
}

static char		*get_word(char **str, char delim)
{
  char			*tmp;
  char			*new;

  while (**str == delim)
    (*str)++;
  if (**str == 0)
    return (NULL);
  tmp = strchr(*str, delim);
  if (!tmp)
    {
      new = strdup(*str);
      while (**str)
	(*str)++;
      return (new);
    }
  my_malloc(new, tmp - *str + 1);
  new[tmp - *str] = '\0';
  strncpy(new, *str, tmp - *str);
  *str = tmp;
  return (new);
}

void			tab_free(char **tab)
{
  int                   index;

  if (!tab)
    return;
  for (index = 0; tab[index]; index++)
    free(tab[index]);
  free(tab);
}

char			**tab_fill(char *str, char delim)
{
  int                   count;
  char                  **tab;
  int                   index;

  if (!str)
    return (NULL);
  count = count_words(str, delim);
  if (!count)
    return (NULL);
  my_malloc(tab, (count + 1) * sizeof(char *));
  for (index = 0; (tab[index] = get_word(&str, delim)); index++)
    ;
  return (tab);
}

/*
**		Deletes the first element of a wordtab, shifting the other
**	elements. The size of the malloced area stays the same, though
*/
int			tab_delete_first(char **tab)
{
  int                   i;

  if (!tab[0])
    return (-1);
  free(tab[0]);
  for (i = 0; tab[i]; i++)
    tab[i] = tab[i + 1];
  return (0);
}

int			tab_count_words(char **tab)
{
  int                   count;

  if (!tab)
    return (0);
  for (count = 0; tab[count]; count++)
    ;
  return (count);
}

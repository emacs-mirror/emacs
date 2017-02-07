#ifndef TEST_COMPAT_H
#define TEST_COMPAT_H

#include <stdio.h>
#include <limits.h>

typedef int Lisp_Object;

void *
xmalloc (size_t size)
{
  return malloc (size);
}

void
xfree (void *ptr)
{
  free (ptr);
}

void *
xrealloc (void *block, size_t size)
{
  return realloc (block, size);
}

void
emacs_abort ()
{
  fprintf (stderr, "Aborting...\n");
  exit (1);
}

#ifndef eassert
#define eassert(cond)                                                   \
  do {                                                                  \
    if (! (cond)) {                                                     \
      fprintf (stderr, "\n%s:%d:eassert condition failed: %s\n",        \
               __FILE__, __LINE__ ,#cond);                              \
      exit (1);                                                         \
    }                                                                   \
  } while (0)
#endif

#ifndef max
#define max(x,y) ((x) >= (y) ? (x) : (y))
#endif
#ifndef min
#define min(x,y) ((x) <= (y) ? (x) : (y))
#endif

#endif

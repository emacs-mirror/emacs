/* Mock necessary parts of lisp.h.

Copyright (C) 2017-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef TEST_COMPAT_H
#define TEST_COMPAT_H

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

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
  exit (EXIT_FAILURE);
}

#ifndef eassert
#define eassert(cond)                                                   \
  do {                                                                  \
    if (! (cond)) {                                                     \
      fprintf (stderr, "%s:%d:eassert condition failed: %s\n",          \
               __FILE__, __LINE__ , # cond);                            \
      exit (EXIT_FAILURE);                                              \
    }                                                                   \
  } while (0)
#endif

#ifndef eassume
#define eassume eassert
#endif

#ifndef max
#define max(x,y) ((x) >= (y) ? (x) : (y))
#endif
#ifndef min
#define min(x,y) ((x) <= (y) ? (x) : (y))
#endif

#endif /* TEST_COMPAT_H */

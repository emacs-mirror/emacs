/* Copyright (C) 1991, 2016-2020 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "exit.h"

#ifdef	HAVE_GNU_LD
CONST struct
  {
    size_t n;
    void EXFUN((*fn[1]), (NOARGS));
  } __libc_atexit;
#endif

/* Call all functions registered with `atexit' and `on_exit',
   in the reverse of the order in which they were registered
   perform stdio cleanup, and terminate program execution with STATUS.  */
__NORETURN
void
DEFUN(exit, (status), int status)
{
  register CONST struct exit_function_list *l;

  for (l = __exit_funcs; l != NULL; l = l->next)
    {
      register size_t i = l->idx;
      while (i-- > 0)
	{
	  CONST struct exit_function *CONST f = &l->fns[i];
	  switch (f->flavor)
	    {
	    case ef_free:
	      break;
	    case ef_on:
	      (*f->func.on.fn)(status, f->func.on.arg);
	      break;
	    case ef_at:
	      (*f->func.at)();
	      break;
	    }
	}
    }

#ifdef	HAVE_GNU_LD
  {
    void EXFUN((*CONST *fn), (NOARGS));
    for (fn = __libc_atexit.fn; *fn != NULL; ++fn)
      (**fn) ();
  }
#else
  {
    extern void EXFUN(_cleanup, (NOARGS));
    _cleanup();
  }
#endif

  _exit(status);
}


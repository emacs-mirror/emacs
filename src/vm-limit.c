/* Functions for memory limit warnings.
   Copyright (C) 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include "lisp.h"
#include "mem_limits.h"

/*
  Level number of warnings already issued.
  0 -- no warnings issued.
  1 -- 75% warning already issued.
  2 -- 85% warning already issued.
*/
static int warnlevel;

/* Function to call to issue a warning;
   0 means don't issue them.  */
static void (*warnfunction) ();

extern POINTER sbrk ();

/* Get more memory space, complaining if we're near the end. */

static POINTER
morecore_with_warning (size)
     register int size;
{
  POINTER result;
  register POINTER cp;
  register unsigned int siz;

  if (!data_space_start)
    {
      data_space_start = start_of_data ();
    }

  if (lim_data == 0)
    get_lim_data ();

  /* Find current end of memory and issue warning if getting near max */
  cp = sbrk (0);
  siz = cp - data_space_start;

  if (warnfunction)
    switch (warnlevel)
      {
      case 0: 
	if (siz > (lim_data / 4) * 3)
	  {
	    warnlevel++;
	    (*warnfunction) ("Warning: past 75% of memory limit");
	  }
	break;

      case 1: 
	if (siz > (lim_data / 20) * 17)
	  {
	    warnlevel++;
	    (*warnfunction) ("Warning: past 85% of memory limit");
	  }
	break;

      case 2: 
	if (siz > (lim_data / 20) * 19)
	  {
	    warnlevel++;
	    (*warnfunction) ("Warning: past 95% of memory limit");
	  }
	break;

      default:
	(*warnfunction) ("Warning: past acceptable memory limits");
	break;
      }

  if (EXCEEDS_ELISP_PTR (cp))
    (*warnfunction) ("Warning: memory in use exceeds lisp pointer size");

  result = sbrk (size);
  if (result == (POINTER) -1)
    return NULL;
  return result;
}

/* Cause reinitialization based on job parameters;
   also declare where the end of pure storage is. */

void
malloc_init (start, warnfun)
     POINTER start;
     void (*warnfun) ();
{
  extern POINTER (* __morecore) ();     /* From gmalloc.c */

  if (start)
    data_space_start = start;
  lim_data = 0;
  warnlevel = 0;
  warnfunction = warnfun;
  __morecore = &morecore_with_warning;
}

/* profile.c --- generate periodic events for profiling of Emacs Lisp code.
   Copyright (C) 1992, 1994, 1999, 2001-2013 Free Software Foundation,
   Inc.

Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


/**
 **  To be run as an emacs subprocess.  Input string that starts with:
 **    'z' -- resets the watch (to zero).
 **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
 **    'q' -- exit.
 **
 **  abstraction : a stopwatch
 **  operations: reset_watch, get_time
 */
#include <config.h>

#define SYSTIME_INLINE EXTERN_INLINE

#include <inttypes.h>
#include <stdio.h>

#include <intprops.h>
#include <systime.h>

static EMACS_TIME TV1;
static int watch_not_started = 1; /* flag */
static char time_string[INT_STRLEN_BOUND (uintmax_t) + sizeof "."
			+ LOG10_EMACS_TIME_RESOLUTION];

/* Reset the stopwatch to zero.  */

static void
reset_watch (void)
{
  TV1 = current_emacs_time ();
  watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<nanoseconds>
   If reset_watch was not called yet, exit.  */

static char *
get_time (void)
{
  EMACS_TIME TV2 = sub_emacs_time (current_emacs_time (), TV1);
  uintmax_t s = EMACS_SECS (TV2);
  int ns = EMACS_NSECS (TV2);
  if (watch_not_started)
    exit (EXIT_FAILURE);  /* call reset_watch first ! */
  sprintf (time_string, "%"PRIuMAX".%0*d", s, LOG10_EMACS_TIME_RESOLUTION, ns);
  return time_string;
}

int
main (void)
{
  int c;
  while ((c = getchar ()) != EOF)
    {
      switch (c)
	{
	case 'z':
	  reset_watch ();
	  break;
	case 'p':
	  puts (get_time ());
	  break;
	case 'q':
	  exit (EXIT_SUCCESS);
	}
      /* Anything remaining on the line is ignored.  */
      while (c != '\n' && c != EOF)
	c = getchar ();
    }
  exit (EXIT_FAILURE);
}


/* profile.c ends here */

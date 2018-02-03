/* profile.c --- generate periodic events for profiling of Emacs Lisp code.
   Copyright (C) 1992, 1994, 1999, 2001-2018 Free Software Foundation,
   Inc.

Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>

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


/**
 **  To be run as an emacs subprocess.  Input string that starts with:
 **    'z' -- resets the watch (to zero).
 **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
 **    'q' -- exit.
 **
 **  abstraction : a stopwatch
 **  operations: reset_watch, get_time
 */

#define INLINE EXTERN_INLINE
#include <config.h>

#include <inttypes.h>
#include <stdlib.h>

#include <intprops.h>
#include <systime.h>
#include <unlocked-io.h>

static struct timespec TV1;
static int watch_not_started = 1; /* flag */
static char time_string[INT_STRLEN_BOUND (uintmax_t) + sizeof "."
			+ LOG10_TIMESPEC_RESOLUTION];

/* Reset the stopwatch to zero.  */

static void
reset_watch (void)
{
  TV1 = current_timespec ();
  watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<nanoseconds>
   If reset_watch was not called yet, exit.  */

static char *
get_time (void)
{
  struct timespec TV2 = timespec_sub (current_timespec (), TV1);
  uintmax_t s = TV2.tv_sec;
  int ns = TV2.tv_nsec;
  if (watch_not_started)
    exit (EXIT_FAILURE);  /* call reset_watch first ! */
  sprintf (time_string, "%"PRIuMAX".%0*d", s, LOG10_TIMESPEC_RESOLUTION, ns);
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
	  return EXIT_SUCCESS;
	}
      /* Anything remaining on the line is ignored.  */
      while (c != '\n' && c != EOF)
	c = getchar ();
    }
  return EXIT_FAILURE;
}


/* profile.c ends here */

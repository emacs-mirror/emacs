/* profile.c --- generate periodic events for profiling of Emacs Lisp code.
 Copyright (C) 1992, 1994 Free Software Foundation, Inc.

 Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>

 This file is part of GNU Emacs.

 GNU Emacs is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 GNU Emacs is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with GNU Emacs; see the file COPYING.  If not, write to
 the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/**
 **  To be run as an emacs process. Input string that starts with:
 **    'z' -- resets the watch (to zero).
 **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
 **    'q' -- exit.
 **
 **  abstraction : a stopwatch
 **  operations: reset_watch, get_time
 */
#include <stdio.h>
#include <../src/config.h>
#include <../src/systime.h>

static EMACS_TIME TV1, TV2;
static int watch_not_started = 1; /* flag */
static char time_string[30];

/* Reset the stopwatch to zero.  */

void
reset_watch ()
{
  EMACS_GET_TIME (TV1);
  watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<micro-seconds> 
   If reset_watch was not called yet, exit.  */

char *
get_time ()
{
  if (watch_not_started)
    exit (1);  /* call reset_watch first ! */
  EMACS_GET_TIME (TV2);
  EMACS_SUB_TIME (TV2, TV2, TV1);
  sprintf (time_string, "%lu.%06lu", EMACS_SECS (TV2), EMACS_USECS (TV2));
  return time_string;
}

void
main ()
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
	  exit (0);
	}
      /* Anything remaining on the line is ignored.  */
      while (c != '\n' && c != EOF)
	c = getchar ();
    }
  exit (1);
}

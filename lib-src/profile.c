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

static struct timeval TV1, TV2;
static struct timezone *tzp = (struct timezone *) NULL; /* no need timezone */
static int watch_not_started = 1; /* flag */
static char time_string[30];

/* Reset the stopwatch to zero.  */

int
reset_watch ()
{
  gettimeofday (&TV1, tzp);
  watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<micro-seconds> 
   If reset_watch was not called yet, return NULL.  */

char *
get_time ()
{
  char *result = time_string;
  int i;
  if (watch_not_started)
    return ((char *) 0); /* call reset_watch first ! */
  gettimeofday (&TV2, tzp);
  if (TV1.tv_usec > TV2.tv_usec)
    {
      TV2.tv_usec += 1000000;
      TV2.tv_sec--;
    }
  sprintf (result,"%lu.%6lu",
	  TV2.tv_sec - TV1.tv_sec, TV2.tv_usec - TV1.tv_usec);
  for (result = index (result, '.') + 1; *result == ' '; result++) 
    *result = '0';
  return time_string;
}

void
main ()
{
  char inp[10];
  while (1)
    {
      gets (inp);
      switch (inp[0])
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
    }
}

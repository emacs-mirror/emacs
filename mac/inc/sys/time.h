/* Replacement sys/time.h file for building GNU Emacs on the Macintosh.
   Copyright (C) 2000, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#ifndef _SYS_TIME_H
#define _SYS_TIME_H

struct timeval {
  long tv_sec;  /* seconds */
  long tv_usec;  /* microseconds */
};

#define ITIMER_REAL      0
#if 0
#define ITIMER_VIRTUAL   1
#define ITIMER_PROF      2
#endif

struct itimerval {
#if 0
  struct timeval it_interval;    /* timer interval */
#endif
  struct timeval it_value;       /* current value */
};

extern int setitimer(int, const struct itimerval *, struct itimerval *);

#endif  /* _SYS_TYPES_H */

/* arch-tag: f85ed04d-0e99-4f97-892b-fe029d0e92f9
   (do not change this comment) */

/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1993-1994, 2002-2017 Free Software Foundation, Inc.

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

#ifndef EMACS_SYSTIME_H
#define EMACS_SYSTIME_H

#include <timespec.h>

INLINE_HEADER_BEGIN

#ifdef emacs
# ifdef HAVE_X_WINDOWS
#  include <X11/X.h>
# else
typedef unsigned long Time;
# endif
#endif

/* On some configurations (hpux8.0, X11R4), sys/time.h and X11/Xos.h
   disagree about the name of the guard symbol.  */
#ifdef HPUX
#ifdef _STRUCT_TIMEVAL
#ifndef __TIMEVAL__
#define __TIMEVAL__
#endif
#endif
#endif

#include <sys/time.h>	/* for 'struct timeval' */

/* Emacs uses struct timespec to represent nonnegative temporal intervals.

   WARNING: Since tv_sec might be an unsigned value, do not use struct
   timespec as a general-purpose data type for adding or subtracting
   arbitrary time values!  When computing A + B or A - B, typically A
   should be an absolute time since the epoch and B a nonnegative offset.  */

/* Return an invalid timespec.  */
INLINE struct timespec
invalid_timespec (void)
{
  return make_timespec (0, -1);
}

/* Return true if TIME is a valid timespec.  This currently doesn't worry
   about whether tv_nsec is less than TIMESPEC_RESOLUTION; leap seconds
   might cause a problem if it did.  */
INLINE bool
timespec_valid_p (struct timespec t)
{
  return t.tv_nsec >= 0;
}

/* Return current system time.  */
INLINE struct timespec
current_timespec (void)
{
  struct timespec r;
  gettime (&r);
  return r;
}

/* defined in sysdep.c */
extern int set_file_times (int, const char *, struct timespec, struct timespec);
extern struct timeval make_timeval (struct timespec) ATTRIBUTE_CONST;

/* defined in keyboard.c */
extern void set_waiting_for_input (struct timespec *);

/* When lisp.h is not included Lisp_Object is not defined (this can
   happen when this file is used outside the src directory).  */
#ifdef emacs

/* Emacs uses the integer list (HI LO US PS) to represent the time
   (HI << LO_TIME_BITS) + LO + US / 1e6 + PS / 1e12.  */
enum { LO_TIME_BITS = 16 };

/* A Lisp time (HI LO US PS), sans the cons cells.  */
struct lisp_time
{
  EMACS_INT hi;
  int lo, us, ps;
};

/* defined in editfns.c */
extern Lisp_Object make_lisp_time (struct timespec);
extern int decode_time_components (Lisp_Object, Lisp_Object, Lisp_Object,
				   Lisp_Object, struct lisp_time *, double *);
extern struct timespec lisp_to_timespec (struct lisp_time);
extern struct timespec lisp_time_argument (Lisp_Object);
#endif

INLINE_HEADER_END

#endif /* EMACS_SYSTIME_H */

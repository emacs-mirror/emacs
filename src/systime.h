/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1993-1994, 2002-2020 Free Software Foundation, Inc.

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

#include "lisp.h"
#include <timespec.h>

INLINE_HEADER_BEGIN

#ifdef HAVE_X_WINDOWS
# include <X11/X.h>
#else
typedef unsigned long Time;
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

#undef hz /* AIX <sys/param.h> #defines this.  */

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
   about whether tv_nsec is less than TIMESPEC_HZ; leap seconds might
   cause a problem if it did.  */
INLINE bool
timespec_valid_p (struct timespec t)
{
  return t.tv_nsec >= 0;
}

/* defined in sysdep.c */
extern int set_file_times (int, const char *, struct timespec, struct timespec);

/* defined in keyboard.c */
extern void set_waiting_for_input (struct timespec *);

/* Emacs uses the integer list (HI LO US PS) to represent the time
   (HI << LO_TIME_BITS) + LO + US / 1e6 + PS / 1e12.  */
enum { LO_TIME_BITS = 16 };

/* Components of a new-format Lisp timestamp.  */
struct lisp_time
{
  /* Clock count as a Lisp integer.  */
  Lisp_Object ticks;

  /* Clock frequency (ticks per second) as a positive Lisp integer.
     (TICKS . HZ) is a valid Lisp timestamp unless HZ < 65536.  */
  Lisp_Object hz;
};

/* defined in timefns.c */
extern struct timeval make_timeval (struct timespec) ATTRIBUTE_CONST;
extern Lisp_Object make_lisp_time (struct timespec);
extern Lisp_Object timespec_to_lisp (struct timespec);
extern bool list4_to_timespec (Lisp_Object, Lisp_Object, Lisp_Object,
			       Lisp_Object, struct timespec *);
extern struct timespec lisp_time_argument (Lisp_Object);
extern AVOID time_overflow (void);
extern void init_timefns (void);
extern void syms_of_timefns (void);

INLINE_HEADER_END

#endif /* EMACS_SYSTIME_H */

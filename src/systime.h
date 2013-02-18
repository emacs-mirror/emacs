/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1993-1994, 2002-2013 Free Software Foundation, Inc.

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

#ifndef EMACS_SYSTIME_H
#define EMACS_SYSTIME_H

#include <timespec.h>

INLINE_HEADER_BEGIN
#ifndef SYSTIME_INLINE
# define SYSTIME_INLINE INLINE
#endif

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

/* The type to use to represent non-negative temporal intervals.  Its
   address can be passed as the timeout argument to the pselect system
   call.  */
typedef struct timespec EMACS_TIME;

/* Resolution of EMACS_TIME time stamps (in units per second), and log
   base 10 of the resolution.  The log must be a positive integer.  */
enum { EMACS_TIME_RESOLUTION = 1000000000 };
enum { LOG10_EMACS_TIME_RESOLUTION = 9 };

/* EMACS_SECS (TIME) is the seconds component of TIME.
   EMACS_NSECS (TIME) is the nanoseconds component of TIME.
   emacs_secs_addr (PTIME) is the address of *PTIME's seconds component.  */
SYSTIME_INLINE time_t EMACS_SECS (EMACS_TIME t) { return t.tv_sec; }
SYSTIME_INLINE int EMACS_NSECS (EMACS_TIME t) { return t.tv_nsec; }
SYSTIME_INLINE time_t *emacs_secs_addr (EMACS_TIME *t) { return &t->tv_sec; }

/* Return an Emacs time with seconds S and nanoseconds NS.  */
SYSTIME_INLINE EMACS_TIME
make_emacs_time (time_t s, int ns)
{
  EMACS_TIME r = { s, ns };
  return r;
}

/* Return an invalid Emacs time.  */
SYSTIME_INLINE EMACS_TIME
invalid_emacs_time (void)
{
  EMACS_TIME r = { 0, -1 };
  return r;
}

/* Return current system time.  */
SYSTIME_INLINE EMACS_TIME
current_emacs_time (void)
{
  EMACS_TIME r;
  gettime (&r);
  return r;
}

/* Return the result of adding A to B, or of subtracting B from A.
   On overflow, store an extremal value: ergo, if time_t is unsigned,
   return 0 if the true answer would be negative.

   WARNING: These are NOT general-purpose macros for adding or
   subtracting arbitrary time values!  They are generally intended to
   be used with their first argument an absolute time since the epoch
   and the second argument a non-negative offset.  Do NOT use them for
   anything else.  */
SYSTIME_INLINE EMACS_TIME
add_emacs_time (EMACS_TIME a, EMACS_TIME b)
{
  return timespec_add (a, b);
}
SYSTIME_INLINE EMACS_TIME
sub_emacs_time (EMACS_TIME a, EMACS_TIME b)
{
  return timespec_sub (a, b);
}

/* Return the sign of the valid time stamp TIME, either -1, 0, or 1.
   Note: this can only return a negative value if time_t is a signed
   data type.  */
SYSTIME_INLINE int
EMACS_TIME_SIGN (EMACS_TIME t)
{
  return timespec_sign (t);
}

/* Return 1 if TIME is a valid time stamp.  */
SYSTIME_INLINE int
EMACS_TIME_VALID_P (EMACS_TIME t)
{
  return 0 <= t.tv_nsec;
}

/* Convert the double D to the greatest EMACS_TIME not greater than D.
   On overflow, return an extremal value; in particular, if time_t is
   an unsigned data type and D is negative, return zero.  Return the
   minimum EMACS_TIME if D is not a number.  */
SYSTIME_INLINE EMACS_TIME
EMACS_TIME_FROM_DOUBLE (double d)
{
  return dtotimespec (d);
}

/* Convert the Emacs time T to an approximate double value D.  */
SYSTIME_INLINE double
EMACS_TIME_TO_DOUBLE (EMACS_TIME t)
{
  return timespectod (t);
}

/* defined in sysdep.c */
extern int set_file_times (int, const char *, EMACS_TIME, EMACS_TIME);
extern struct timeval make_timeval (EMACS_TIME);

/* defined in keyboard.c */
extern void set_waiting_for_input (EMACS_TIME *);

/* When lisp.h is not included Lisp_Object is not defined (this can
   happen when this files is used outside the src directory).
   Use GCPRO1 to determine if lisp.h was included.  */
#ifdef GCPRO1
/* defined in editfns.c */
extern Lisp_Object make_lisp_time (EMACS_TIME);
extern bool decode_time_components (Lisp_Object, Lisp_Object, Lisp_Object,
				    Lisp_Object, EMACS_TIME *, double *);
extern EMACS_TIME lisp_time_argument (Lisp_Object);
#endif

/* Compare times T1 and T2 for equality, inequality etc.  */
SYSTIME_INLINE int
EMACS_TIME_EQ (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) == 0;
}
SYSTIME_INLINE int
EMACS_TIME_NE (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) != 0;
}
SYSTIME_INLINE int
EMACS_TIME_GT (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) > 0;
}
SYSTIME_INLINE int
EMACS_TIME_GE (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) >= 0;
}
SYSTIME_INLINE int
EMACS_TIME_LT (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) < 0;
}
SYSTIME_INLINE int
EMACS_TIME_LE (EMACS_TIME t1, EMACS_TIME t2)
{
  return timespec_cmp (t1, t2) <= 0;
}

INLINE_HEADER_END

#endif /* EMACS_SYSTIME_H */

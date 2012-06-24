/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1993-1994, 2002-2012 Free Software Foundation, Inc.

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

#ifdef WINDOWSNT
#include <sys/time.h>	/* for 'struct timeval' */
#endif

/* The type to use to represent temporal intervals.  It can be passed
   as the timeout argument to the pselect system call.  */
#define EMACS_TIME struct timespec

/* Resolution of EMACS_TIME time stamps (in units per second), and log
   base 10 of the resolution.  The log must be a positive integer.  */
#define EMACS_TIME_RESOLUTION		1000000000
#define LOG10_EMACS_TIME_RESOLUTION	9

/* EMACS_SECS (TIME) is an rvalue for the seconds component of TIME.
   EMACS_SECS_ADDR (time) is the address of the seconds component.
   EMACS_SET_SECS (TIME, SECONDS) sets that to SECONDS.

   EMACS_NSECS (TIME) is an rvalue for the nanoseconds component of TIME.
   EMACS_SET_NSECS (TIME, NANOSECONDS) sets that to NANOSECONDS.

   EMACS_SET_SECS_NSECS (TIME, SECS, NSECS) sets both components of TIME.  */
#define EMACS_SECS(time)		    ((time).tv_sec  + 0)
#define EMACS_NSECS(time)		    ((time).tv_nsec + 0)
#define EMACS_SECS_ADDR(time)		    (&(time).tv_sec)
#define EMACS_SET_SECS(time, seconds)	    ((time).tv_sec  = (seconds))
#define EMACS_SET_NSECS(time, ns)	    ((time).tv_nsec = (ns))
#define EMACS_SET_SECS_NSECS(time, s, ns)				\
  ((void) (EMACS_SET_SECS (time, s), EMACS_SET_NSECS (time, ns)))

/* Convenience macros for older code that counts microseconds.  */
#define EMACS_SET_USECS(time, us)  ((void) EMACS_SET_NSECS (time, (us) * 1000))
#define EMACS_SET_SECS_USECS(time, secs, usecs) 		\
  (EMACS_SET_SECS (time, secs), EMACS_SET_USECS (time, usecs))

/* Set TIME to an invalid time stamp.  */
#define EMACS_SET_INVALID_TIME(time)	    EMACS_SET_SECS_NSECS(time, 0, -1)

/* Set TIME to the current system time.  */
#define EMACS_GET_TIME(time)		    gettime (&(time))

/* Put into DEST the result of adding SRC1 to SRC2, or of subtracting
   SRC2 from SRC1.  On overflow, store an extremal value.  */
#define EMACS_ADD_TIME(dest, src1, src2) ((dest) = timespec_add (src1, src2))
#define EMACS_SUB_TIME(dest, src1, src2) ((dest) = timespec_sub (src1, src2))

/* Return the sign of the valid time stamp TIME, either -1, 0, or 1.  */
#define EMACS_TIME_SIGN(time)		timespec_sign (time)

/* Return 1 if TIME is a valid time stamp.  */
#define EMACS_TIME_VALID_P(time)	(0 <= (time).tv_nsec)

/* Convert the double D to the greatest EMACS_TIME not greater than D.
   On overflow, return an extremal value.  Return the minimum
   EMACS_TIME if D is not a number.  */
#define EMACS_TIME_FROM_DOUBLE(d)	dtotimespec (d)

/* Convert the Emacs time T to an approximate double value D.  */
#define EMACS_TIME_TO_DOUBLE(t)		timespectod (t)

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
extern int decode_time_components (Lisp_Object, Lisp_Object, Lisp_Object,
				   Lisp_Object, EMACS_TIME *, int *);
extern EMACS_TIME lisp_time_argument (Lisp_Object, int *);
#endif

/* Compare times T1 and T2 for equality, inequality etc.  */

#define EMACS_TIME_EQ(T1, T2) (timespec_cmp (T1, T2) == 0)
#define EMACS_TIME_NE(T1, T2) (timespec_cmp (T1, T2) != 0)
#define EMACS_TIME_GT(T1, T2) (timespec_cmp (T1, T2) > 0)
#define EMACS_TIME_GE(T1, T2) (timespec_cmp (T1, T2) >= 0)
#define EMACS_TIME_LT(T1, T2) (timespec_cmp (T1, T2) < 0)
#define EMACS_TIME_LE(T1, T2) (timespec_cmp (T1, T2) <= 0)

#endif /* EMACS_SYSTIME_H */

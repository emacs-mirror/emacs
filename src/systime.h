/* systerm.h - System-dependent definitions for time manipulations.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

#ifdef NEED_TIME_H

/* _h_BSDTYPES is checked because on ISC unix, socket.h includes
   both time.h and sys/time.h, and the later file is protected
   from repeated inclusion.  We just hope that other systems will
   use this guard either not at all, or similarly.  */
#ifndef _h_BSDTYPES
#include <time.h>
#endif
#else /* not NEED_TIME_H */
#ifdef HAVE_TIMEVAL
#include <sys/time.h>
#endif /* HAVE_TIMEVAL */
#endif /* not NEED_TIME_H */

/* EMACS_TIME is the type to use to represent temporal intervals -
   struct timeval on some systems, int on others.  It can be passed as
   the timeout argument to the select () system call.

   EMACS_SECS (TIME) is an rvalue for the seconds component of TIME.
   EMACS_SET_SECS (TIME, SECONDS) sets that to SECONDS.

   EMACS_HAS_USECS is defined iff EMACS_TIME has a usecs component.
   EMACS_USECS (TIME) is an rvalue for the milliseconds component of TIME.
   	This returns zero if EMACS_TIME doesn't have a milliseconds component.
   EMACS_SET_USECS (TIME, MILLISECONDS) sets that to MILLISECONDS.
	This does nothing if EMACS_TIME doesn't have a milliseconds component.

   EMACS_SET_SECS_USECS (TIME, SECS, USECS) sets both components of TIME.

   EMACS_GET_TIME (TIME) stores the current system time in TIME, which
	should be an lvalue.
   EMACS_SET_UTIMES (PATH, ATIME, MTIME) changes the last-access and
	last-modification times of the file named PATH to ATIME and
	MTIME, which are EMACS_TIMEs.

   EMACS_ADD_TIME (DEST, SRC1, SRC2) adds SRC1 to SRC2 and stores the
	result in DEST.  SRC should not be negative.

   EMACS_SUB_TIME (DEST, SRC1, SRC2) subtracts SRC2 from SRC1 and
	stores the result in DEST.  SRC should not be negative. 
   EMACS_TIME_NEG_P (TIME) is true iff TIME is negative.

*/

#ifdef HAVE_TIMEVAL

#define EMACS_TIME struct timeval
#define EMACS_SECS(time)		    ((time).tv_sec  + 0)
#define EMACS_USECS(time)		    ((time).tv_usec + 0)
#define EMACS_SET_SECS(time, seconds)	    ((time).tv_sec  = (seconds))
#define EMACS_SET_USECS(time, milliseconds) ((time).tv_usec = (milliseconds))

#define EMACS_GET_TIME(time)					\
{								\
  EMACS_TIME dummy;						\
  gettimeofday (&(time), &dummy);				\
}

#define EMACS_ADD_TIME(dest, src1, src2)			\
{								\
  (dest).tv_sec  = (src1).tv_sec  + (src2).tv_sec;		\
  (dest).tv_usec = (src1).tv_usec + (src2).tv_usec;		\
  if ((dest).tv_usec > 1000000)					\
    (dest).tv_usec -= 1000000, (dest).tv_sec++;			\
}

#define EMACS_SUB_TIME(dest, src1, src2)			\
{								\
  (dest).tv_sec  = (src1).tv_sec  - (src2).tv_sec;		\
  (dest).tv_usec = (src1).tv_usec - (src2).tv_usec;		\
  if ((dest).tv_usec < 0)					\
    (dest).tv_usec += 1000000, (dest).tv_sec--;			\
}

#define EMACS_TIME_NEG_P(time)					\
  ((time).tv_sec < 0						\
   || ((time).tv_sec == 0					\
       && (time).tv_usec < 0))

#else /* not def HAVE_TIMEVAL */

#define EMACS_TIME int
#define EMACS_SECS(time)		    (time)
#define EMACS_SET_SECS(time, seconds)	    ((time) = (seconds))

#define EMACS_GET_TIME(t) ((t) = time ((long *) 0))
#define EMACS_ADD_TIME(dest, src1, src2) ((dest) = (src1) + (src2))
#define EMACS_SUB_TIME(dest, src1, src2) ((dest) = (src1) - (src2))
#define EMACS_TIME_NEG_P(t) ((t) < 0)

#endif /* def HAVE_TIMEVAL */

#define EMACS_SET_SECS_USECS(time, secs, usecs) 		\
  (EMACS_SET_SECS (time, secs), EMACS_SET_USECS (time, usecs))

#ifdef USE_UTIME

#define EMACS_SET_UTIMES(path, atime, mtime)			\
  {								\
    struct time_t tv[2];					\
    tv[0] = EMACS_SECS (atime);					\
    tv[1] = EMACS_SECS (mtime);					\
    utime ((path), tv);						\
  }

#else

#define EMACS_SET_UTIMES(path, atime, mtime)			\
  {								\
    EMACS_TIME tv[2];						\
    tv[0] = atime;						\
    tv[1] = mtime;						\
    utimes ((path), tv);					\
  }

#endif

/* systime.h - System-dependent definitions for time manipulations.
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

#if defined (HAVE_TIMEVAL) && !defined (NEED_TIME_H)
/* NEED_TIME_H is necessary because some versions of HP/UX shouldn't
   have this included; time.h should do the trick instead.  */

#include <sys/time.h>

#else

/* _h_BSDTYPES is checked because on ISC unix, socket.h includes
   both time.h and sys/time.h, and the later file is protected
   from repeated inclusion.  We just hope that other systems will
   use this guard either not at all, or similarly.  */
#ifndef _h_BSDTYPES
#include <time.h>
#endif /* _h_BSDTYPES */

#endif

/* AIX and SCO 3.2v4 need both <sys/time.h> and <time.h>.  */
#if defined (_AIX) || defined (SCO)
#include <time.h>
#endif

/* SVr4 doesn't actually declare this in its #include files.  */
#ifdef USG5_4
extern long timezone;
#endif

#ifdef VMS
#ifdef VAXC
#include "vmstime.h"
#endif
#endif


/* EMACS_TIME is the type to use to represent temporal intervals -
   struct timeval on some systems, int on others.  It can be passed as
   the timeout argument to the select  system call.

   EMACS_SECS (TIME) is an rvalue for the seconds component of TIME.
   EMACS_SET_SECS (TIME, SECONDS) sets that to SECONDS.

   EMACS_HAS_USECS is defined iff EMACS_TIME has a usecs component.
   EMACS_USECS (TIME) is an rvalue for the microseconds component of TIME.
   	This returns zero if EMACS_TIME doesn't have a microseconds component.
   EMACS_SET_USECS (TIME, MICROSECONDS) sets that to MICROSECONDS.
	This does nothing if EMACS_TIME doesn't have a microseconds component.

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

#define EMACS_HAS_USECS

#define EMACS_TIME struct timeval
#define EMACS_SECS(time)		    ((time).tv_sec  + 0)
#define EMACS_USECS(time)		    ((time).tv_usec + 0)
#define EMACS_SET_SECS(time, seconds)	    ((time).tv_sec  = (seconds))
#define EMACS_SET_USECS(time, microseconds) ((time).tv_usec = (microseconds))

#define EMACS_GET_TIME(time)					\
{								\
  struct timezone dummy;					\
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

#else /* ! defined (HAVE_TIMEVAL) */

#define EMACS_TIME int
#define EMACS_SECS(time)		    (time)
#define EMACS_USECS(time)		    0
#define EMACS_SET_SECS(time, seconds)	    ((time) = (seconds))
#define EMACS_SET_USECS(time, usecs)	    0

#define EMACS_GET_TIME(t) ((t) = time ((long *) 0))
#define EMACS_ADD_TIME(dest, src1, src2) ((dest) = (src1) + (src2))
#define EMACS_SUB_TIME(dest, src1, src2) ((dest) = (src1) - (src2))
#define EMACS_TIME_NEG_P(t) ((t) < 0)

#endif /* ! defined (HAVE_TIMEVAL) */

#define EMACS_SET_SECS_USECS(time, secs, usecs) 		\
  (EMACS_SET_SECS (time, secs), EMACS_SET_USECS (time, usecs))

#ifdef USE_UTIME

#define EMACS_SET_UTIMES(path, atime, mtime)			\
  {								\
    time_t tv[2];						\
    tv[0] = EMACS_SECS (atime);					\
    tv[1] = EMACS_SECS (mtime);					\
    utime ((path), tv);						\
  }

#else /* ! defined (USE_UTIME) */

#define EMACS_SET_UTIMES(path, atime, mtime)			\
  {								\
    EMACS_TIME tv[2];						\
    tv[0] = atime;						\
    tv[1] = mtime;						\
    utimes ((path), tv);					\
  }

#endif /* ! defined (USE_UTIME) */



/* EMACS_CURRENT_TIME_ZONE (int *OFFSET, int *SAVINGS_FLAG,
                            char *STANDARD_ABBR, char *SAVINGS_ABBR);
   expands to a statement which stores information about the current
   time zone in its arguments.

   *OFFSET is set to the number of minutes EAST of Greenwich at which
   the site's time zone is located.  This should describe the offset
   to standard time only; if some sort of daylight savings time is in
   effect, that should not affect this value.  Note that the tm_gmtoff
   member of the struct tm returned by localtime is adjusted for
   daylight savings, so you don't want to use localtime to set
   *OFFSET; gettimeofday does the right thing.

   *SAVINGS_FLAG is set to 1 if some sort of daylight savings time is
   currently in effect, or 0 if no seasonal adjustment is currently
   active.

   *STANDARD_ABBR points to an array of at least 10 characters, which
   should be set to the standard abbreviation for the time zone name
   when daylight savings time is not active.  For example, EDT would
   be appropriate for the Eastern time zone of the USA.

   *SAVINGS_ABBR points to an array of at least 10 characters, which
   should be set to the standard abbreviation for the time zone name
   when daylight savings time is active.  For example, EST would be
   appropriate for the Eastern time zone of the USA.

   If the operating system cannot provide all this information, then
   this macro will not be defined.  */


/* The operating system configuration file can define
   EMACS_CURRENT_TIME_ZONE.   If not, we'll take a shot at it here.  */

#ifndef EMACS_CURRENT_TIME_ZONE

/* System V derivatives have a timezone global variable.  */
#if defined(USG) || defined(VMS)
#define EMACS_GET_TZ_OFFSET(offset)					\
  do {									\
    tzset ();								\
    *(offset) = timezone;						\
  } while (0)
#endif

/* If we have timeval, then we have gettimeofday; that's half the battle.  */
#if defined (HAVE_TIMEVAL) && !defined (EMACS_GET_TZ_OFFSET)
#define EMACS_GET_TZ_OFFSET(offset)					\
  do {									\
    struct timeval dummy;						\
    struct timezone zoneinfo;						\
									\
    gettimeofday (&dummy, &zoneinfo);					\
    *(offset) = -zoneinfo.tz_minuteswest;				\
  } while (0)
#endif /* ! defined (HAVE_TIMEVAL) */

/* The following sane systems have a tzname array.  The timezone function
   is a stupid idea; timezone names can only be determined geographically,
   not by Greenwich offset.  */
#if defined (ultrix) || defined (hpux) || defined (_AIX) || defined (USG) || defined(VMS)

#define EMACS_GET_TZ_NAMES(standard, savings)				\
  do {									\
    extern char *tzname[2];						\
    strcpy ((standard), tzname[0]);					\
    strcpy ((savings), tzname[1]);					\
  } while (0)

#else /* ! defined (ultrix) || defined (hpux) || defined (_AIX) */
/* If we are running SunOS, Mt. Xinu BSD, or MACH 2.5, these systems have a
   timezone function.  */
#if (defined (hp9000) && ! defined (hpux) && defined (unix)) || defined (MACH) || defined (sun) || defined (NeXT)

#define EMACS_GET_TZ_NAMES(standard, savings)				\
  do {									\
    struct timeval dummy;						\
    struct timezone zoneinfo;						\
    extern char *timezone ();						\
									\
    gettimeofday (&dummy, &zoneinfo);					\
    strcpy ((standard), timezone (zoneinfo.tz_minuteswest, 0));		\
    strcpy ((savings),  timezone (zoneinfo.tz_minuteswest, 1));		\
  } while (0)

#endif /* ! (defined (hp9000) && ! defined (hpux) && defined (unix)) || defined (MACH) || defined (sun) */
#endif /* ! defined (ultrix) || defined (hpux) || defined (_AIX) */

/* If we can get all the information we need, let's define the macro!  */
#if defined (EMACS_GET_TZ_OFFSET) && defined (EMACS_GET_TZ_NAMES)

#define EMACS_CURRENT_TIME_ZONE(offset, savings_flag, standard, savings)\
  do {								       	\
    EMACS_TIME t;							\
    long secs;								\
    struct tm *tmp;							\
									\
    EMACS_GET_TIME (t);							\
    secs = EMACS_SECS (t);						\
    tmp = localtime (&secs);						\
    *(savings_flag) = tmp->tm_isdst;					\
									\
    EMACS_GET_TZ_OFFSET (offset);					\
    EMACS_GET_TZ_NAMES (standard, savings);				\
  } while (0)
#endif /* ! defined (EMACS_GET_TZ_OFFSET) && defined (EMACS_GET_TZ_NAMES) */

#endif /* EMACS_CURRENT_TIME_ZONE */

/* Definitions file for GNU Emacs running on Sequent DYNIX/ptx 1.x/2.x
   Copyright (C) 1987, 1990 Free Software Foundation, Inc.

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

/* This file was written by Bill Burton <billb@progress.com>.  Parts were
   adapted from m-ptx1-2.h and process.c as distributed with the Emacs 18.57
   on the Sequent Public software tape. Other parts were adapted from
   usg5-4.h. */

/* Use the SysVr3 file for base configuration even though much is changed.  */
#include "usg5-3.h"

/* Undo these defines because they are incorrect or need to be changed.  */
#undef LIB_X11_LIB
#undef LIBX10_SYSTEM
#undef LIBX11_SYSTEM
#undef USG_SHARED_LIBRARIES

/* <sys/stat.h> *defines* stat as a static function.  If "static"
   is blank, then many files will have a public definition for stat.  */
#undef static

/* PTX supports job control.  */
#undef NOMULTIPLEJOBS

/* PTX has System V streams.  */
#define SYSV_STREAMS
 
/* Leave out -lPW since it conflicts with term.o and because we're not sure 
   if the alloca found there by autoconf should be trusted on PTX.  */
#define LIB_STANDARD -lc

/* Local define.  If TCP/IP is not installed, comment this out.  */
#define TCPIP_INSTALLED

#ifdef TCPIP_INSTALLED
#define HAVE_SOCKETS
#else
#undef subprocesses
#endif

#ifdef HAVE_X_WINDOWS

#define LIBX11_SYSTEM -lsocket -linet -lnsl
/* This is also defined so that lib-src/profile can link.  */
#define LIBS_SYSTEM -lseq

#else /* ! HAVE_X_WINDOWS */

#ifdef HAVE_SOCKETS
#define LIBS_SYSTEM -lsocket -linet -lnsl -lseq
#else
#define LIBS_SYSTEM -lseq
#endif

#endif /* ! HAVE_X_WINDOWS */

/* No <sioctl.h> */
#define NO_SIOCTL_H

/* If we have X windows, configure should find gettimeofday in -lX11.
   Since we emulate gettimeofday below, we really have it anyway.  */
#ifndef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY
#endif

#ifdef emacs
#include <sys/stropts.h>	/* Support for pty's */
#include <sys/conf.h>

/*#undef SIGIO*/		/* SIGIO is already undef'd elsewhere. PTX
                                   has SIGIO, but it's just an alias for
                                   SIGPOLL.  */

/* Emulate gettimeofday() except for the time zone information which Emacs
   doesn't use anyway.  Get_process_stats() is in -lseq.  */
#include <sys/procstats.h>
#define gettimeofday(tp, tzp) get_process_stats (tp, PS_SELF, 0, 0)

/* Define timezone since it's not in sys/time.h.  Unfortunately, this causes
   trouble when building with X since this struct is defined in
   <X11/Xos.h>.  */
struct timezone
{
  int tz_minuteswest;
  int tz_dsttime;
};

/* Unfortunately, this define is not checked in all files including
   <X11/Xos.h> so we can't use it.  */
/* #define XOS_NEEDS_TIME_H */

/* In ptx/WINDOWS, this prevents problems with the timezone struct being
   redefined in <X11/Xos.h>.  It seems the necessary include files are
   included via systime.h so leaving them out here is not a problem.  This
   may not work in X11R5 or X11R6.  */
#define __TIMEVAL__

#endif  /* emacs */

/* PTX doesn't have FIONREAD at all. */
#undef INTERRUPT_INPUT
#define BROKEN_FIONREAD

/* We can support this */
#define CLASH_DETECTION

/* PTX has termios */
#define HAVE_TERMIOS
#undef HAVE_TERMIO
#undef BROKEN_TIOCGWINSZ
#undef BROKEN_TIOCGETC

/* It is possible to receive SIGCHLD when there are no children
   waiting, because a previous waitsys cleaned up the carcass of child
   without clearing the SIGCHLD pending info.  So, use a non-blocking
   wait3 instead, which maps to waitpid in SysVr4.  */
/* Not sure if this is used but PTX does support waitpid.  */
/*#define HAVE_WAIT_HEADER*/
/*#define WAITTYPE int*/
#define wait3(status, options, rusage) \
  waitpid ((pid_t) -1, (status), (options))
/*#define WRETCODE(w) (w >> 8)*/

/* PTX has pty's but not like System V */
#define HAVE_PTYS
#undef SYSV_PTYS

/* Provide pty support which is defined into process.c:allocate_pty.
   Basic ideas for handling getpseudotty were lifted from process.c in
   Emacs 18.57 included on the Sequent Public Software tape.  However, this
   implementation bears almost no resemblance to the original and does not
   require that process.c be patched.  */
#define PTY_ITERATION						\
  char *mastername, *slavename;					\
  while (1)

#define PTY_OPEN						\
  if (failed_count++ >= 5) break;				\
  if ((fd = getpseudotty (&slavename, &mastername)) < 0) {	\
    error("Out of ptys.");					\
    continue;							\
  }								\
  strcpy (pty_name, slavename);

/* Define these to prevent the default logic in process.c:allocate_pty 
   from being used.  */
#define PTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF

/* PTX doesn't seem to have memmove.  */
#define MEMMOVE_MISSING

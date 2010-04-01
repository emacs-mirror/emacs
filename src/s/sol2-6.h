/* Definitions file for GNU Emacs running on Solaris 2.6.

   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010  Free Software Foundation, Inc.

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

#include "usg5-4.h"

#define SOLARIS2

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

#define LIBS_SYSTEM -lsocket -lnsl -lkstat

/* Prefer kstat over kvm in getloadavg.c, kstat doesn't require root.
   ghazi@caip.rutgers.edu, 7/21/97.  Don't redefine if already defined
   (e.g., by config.h). */
#ifndef HAVE_LIBKSTAT
#define HAVE_LIBKSTAT
#endif

/* inoue@ainet.or.jp says Solaris has a bug related to X11R6-style
   XIM support.  */

#define INHIBIT_X11R6_XIM

/* Must use the system's termcap, if we use any termcap.
   It does special things.  */

#ifndef TERMINFO
#define LIBS_TERMCAP -ltermcap
#endif

/* This is the same definition as in usg5-4.h, but with sigblock/sigunblock
   rather than sighold/sigrelse, which appear to be BSD4.1 specific and won't
   work if POSIX_SIGNALS is defined.  It may also be appropriate for SVR4.x
   (x<2) but I'm not sure.   fnf@cygnus.com */
/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname (), *ptyname;			\
						\
    sigblock (sigmask (SIGCLD));		\
    if (grantpt (fd) == -1)			\
      { emacs_close (fd); return -1; }		\
    sigunblock (sigmask (SIGCLD));		\
    if (unlockpt (fd) == -1)			\
      { emacs_close (fd); return -1; }		\
    if (!(ptyname = ptsname (fd)))		\
      { emacs_close (fd); return -1; }		\
    strncpy (pty_name, ptyname, sizeof (pty_name)); \
    pty_name[sizeof (pty_name) - 1] = 0;	\
  }

/* `#ifdef USE_MOTIF' won't work here, since USE_MOTIF isn't defined yet.
   Instead, dynamically check whether USE_MOTIF expands to something.  */
#define NOT_USING_MOTIF { set x USE_MOTIF; test "$$2" = "USE_MOTIF"; }

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM_TEMACS -L/usr/ccs/lib LD_SWITCH_X_SITE_AUX \
  `NOT_USING_MOTIF || echo ' -R/usr/dt/lib'`
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in temacs.  */
#define LD_SWITCH_SYSTEM_TEMACS -L/usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX` \
  `NOT_USING_MOTIF || echo ' -R/usr/dt/lib -L/usr/dt/lib'`

/* Get rid of -traditional and let const really do its thing.  */
#undef C_SWITCH_SYSTEM
#undef const
#endif /* GCC */

/* Gregory Neil Shapiro <gshapiro@hhmi.org> reports the Motif header files
   are in this directory on Solaris 2.4.  */
#define C_SWITCH_X_SYSTEM -I/usr/dt/include

/* -lgen is needed for the regex and regcmp functions
   which are used by Motif.  In the future we can try changing
   regex.c to provide them in Emacs, but this is safer for now.  */
#define LIB_MOTIF -lXm -lgen

/* This is the only known way to avoid some crashes
   that seem to relate to screwed up malloc data
   after deleting a frame.  */
/* rms: I think the problems using ralloc had to do with system
   libraries that called the system malloc even if we linked in the
   GNU malloc.  I could not see any way to fix the problem except to
   have just one malloc and that had to be the system one.  */
/* This is not always necessary.  Turned off at present for testers to
   identify any problems with gmalloc more accurately.  */
/* #define SYSTEM_MALLOC */

/* Probably OK also on earlier versions.  */
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: 71ea3857-89dc-4395-9623-77964e6ed3ca
   (do not change this comment) */

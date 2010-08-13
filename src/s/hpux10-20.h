/* System description file for hpux version 10.20.
   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
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


#define RUN_TIME_REMAP

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG				/* System III, System V, etc */

#define USG5

#define HPUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "hpux"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

#define FIRST_PTY_LETTER 'p'

#define HAVE_TERMIOS
#define NO_TERMIO

#define ORDINARY_LINK

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/* Define this symbol if your system has the functions bcopy, etc.
 * s800 and later versions of s300 (s200) kernels have equivalents
 * of the BSTRING functions of BSD.  If your s200 kernel doesn't have
 * em comment out this section.
 */

#define BSTRING

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* The symbol in the kernel where the load average is found
   depends on the cpu type, so we let the m- files define LDAV_SYMBOL.  */

/* Special hacks needed to make Emacs run on this system.  */

/* Use the system provided termcap(3) library */
#define TERMINFO

/* In hpux, the symbol SIGIO is defined, but the feature
   doesn't work in the way Emacs needs it to.  */

#define BROKEN_SIGIO

/* Some additional system facilities exist.  */

#define HAVE_PERROR  /* Delete this line for version 6.  */

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
            sprintf (pty_name, "/dev/pty/tty%c%x", c, i);

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF \
	sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

/* Don't use shared libraries.  unexec doesn't handle them.
   Note GCC automatically passes -a archive to ld, and it has its own
   conflicting -a.  */
#define LD_SWITCH_SYSTEM_TEMACS

#ifndef HAVE_LIBXMU
/* HP-UX doesn't supply Xmu.  */
#define LIBXMU

#endif

/* Assar Westerlund <assar@sics.se> says this is necessary for
   HP-UX 10.20, and that it works for HP-UX 0 as well.  */
#define NO_EDITRES

/* Tested in getloadavg.c.  */
#define HAVE_PSTAT_GETDYNAMIC

/* Eric Backus <ericb@lsid.hp.com> says, HP-UX 9.x on HP 700 machines
   has a broken `rint' in some library versions including math library
   version number A.09.05.

   You can fix the math library by installing patch number PHSS_4630.
   But we can fix it more reliably for Emacs like this. */
#undef HAVE_RINT

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef random
#undef HAVE_RANDOM

/* AlainF 20-Jul-1996 says this is right.  */
#define KERNEL_FILE "/stand/vmunix"

#define LIBS_SYSTEM -l:libdld.sl

/* Rainer Malzbender <rainer@displaytech.com> says definining
   HAVE_XRMSETDATABASE allows Emacs to compile on HP-UX 10.20
   using GCC.  */

#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

/* Make sure we get select from libc rather than from libcurses
   because libcurses on HPUX 10.10 has a broken version of select.
   We used to use -lc -lcurses, but this may be cleaner.  */
#define LIBS_TERMCAP -ltermcap

/* However, HPUX 10 puts Xaw and Xmu in a strange place
   (if you install them at all).  So search that place.  */
#define C_SWITCH_X_SYSTEM  -I/usr/include/X11R6 -I/usr/include/X11R5 -I/usr/include/Motif1.2 -I/usr/contrib/X11R6/include -I/usr/contrib/X11R5/include
#define LD_SWITCH_X_DEFAULT -L/usr/lib/X11R6 -L/usr/lib/X11R5 -L/usr/lib/Motif1.2 -L/usr/contrib/X11R5/lib

/* 2000-11-21: Temporarily disable Unix 98 large file support found by
   configure.  It fails on HPUX 11, at least, because it enables
   header sections which lose when `static' is defined away, as it is
   on HP-UX.  (You get duplicate symbol errors on linking). */

#undef _FILE_OFFSET_BITS

/* otherwise sigunblock wont be defined */
#define POSIX_SIGNALS

/* arch-tag: 8d8dcbf1-ca9b-48a1-94be-b750de18a5c6
   (do not change this comment) */

/* Definitions file for GNU Emacs running on AT&T's System V Release 4
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

/* This file written by James Van Artsdalen of Dell Computer Corporation.
 * james@bigtex.cactus.org.  Subsequently improved for Dell 2.2 by Eric
 * S. Raymond <esr@snark.thyrsus.com>.
 */

/* Use the SysVr3 file for at least base configuration. */

#include "usg5-3.h"

#define USG5_4

/* We do have multiple jobs.  Handle ^Z. */

#undef NOMULTIPLEJOBS

#define LIBS_SYSTEM -lsocket -lnsl -lelf
#define ORDINARY_LINK

#if 0
#ifdef ORDINARY_LINK
#define LIB_STANDARD -lc /usr/ucblib/libucb.a
#else
#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o
#define LIB_STANDARD -lc /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o
#endif
#else

#ifdef ORDINARY_LINK
#define LIB_STANDARD
#else
#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o
#define LIB_STANDARD -lc /usr/ccs/lib/crtn.o
#endif
#endif

/* there are no -lg libraries on this system, and no libPW */

#define LIBS_DEBUG
#define LIBS_STANDARD -lc

/* No <sioctl.h> */

#define NO_SIOCTL_H

/* Undump with ELF */

#undef COFF

#define UNEXEC unexelf.o

/* Get FIONREAD from <sys/filio.h>.  Get <sys/ttold.h> to get struct
 * tchars. But get <termio.h> first to make sure ttold.h doesn't
 * interfere.  And don't try to use SIGIO yet.
 */

#ifdef emacs
#include <sys/filio.h>
#include <termio.h>
#include <sys/ttold.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/termios.h>
#undef SIGIO
#endif

/* Some SVr4s don't define NSIG in sys/signal.h for ANSI environments;
 * instead, there's a system variable _sys_nsig.  Unfortunately, we need the
 * constant to dimension an array.  So wire in the appropriate value here.
 */

#ifndef NSIG
#define NSIG	32
#endif

/* libc has this stuff, but not utimes. */

#define HAVE_SELECT
#define HAVE_TIMEVAL

/* <sys/stat.h> *defines* stat(2) as a static function.  If "static"
 * is blank, then many files will have a public definition for stat(2).
 */

#undef static

/* We need bss_end from emacs.c for undumping */

#ifndef USG_SHARED_LIBRARIES
#define USG_SHARED_LIBRARIES
#endif

/* We can support this */

#define CLASH_DETECTION

#define HAVE_PTYS
#define HAVE_SETSID
#define HAVE_TERMIOS
#undef BROKEN_TIOCGWINSZ
#undef BROKEN_TIOCGETC

/* It is possible to receive SIGCHLD when there are no children
   waiting, because a previous waitsys(2) cleaned up the carcass of child
   without clearing the SIGCHLD pending info.  So, use a non-blocking
   wait3 instead, which maps to waitpid(2) in SysVr4. */

#define HAVE_WAIT_HEADER
#define WAITTYPE int
#define wait3(status, options, rusage) \
  waitpid((pid_t) -1, (status), (options))
#define WRETCODE(w) (w >> 8)

/* TIOCGPGRP is broken in SysVr4, so we can't send signals to PTY
   subprocesses the usual way.  But TIOCSIGNAL does work for PTYs, and
   this is all we need.  */

#define TIOCSIGSEND TIOCSIGNAL

/* This change means that we don't loop through allocate_pty too many
   times in the (rare) event of a failure. */

#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'z'

/* This sets the name of the master side of the PTY. */

#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");

/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname(), *ptyname;			\
						\
    sighold(SIGCLD);				\
    if (grantpt(fd) == -1)			\
      fatal("could not grant slave pty");	\
    sigrelse(SIGCLD);				\
    if (unlockpt(fd) == -1)			\
      fatal("could not unlock slave pty");	\
    if (!(ptyname = ptsname(fd)))		\
      fatal ("could not enable slave pty");	\
    strncpy(pty_name, ptyname, sizeof(pty_name)); \
    pty_name[sizeof(pty_name) - 1] = 0;		\
  }

/* Push various streams modules onto a PTY channel. */

#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1)	\
    fatal ("ioctl I_PUSH ptem", errno);		\
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) \
    fatal ("ioctl I_PUSH ttcompat", errno);

/* Undo the SVr3 X11 library definition */
#undef LIB_X11_LIB

/* The definition of this in s-usg5-3.h is not needed in 5.4.  */
/* liblnsl_s should never be used.  The _s suffix implies a shared
   library, as opposed to a DLL.  Share libraries were used in SVR3, and are
   available only in order to allow SVR3 binaries to run.  They should not be
   linked in to new binaries. -- caraway!pinkas@caraway.intel.com.  */
#undef LIBX10_SYSTEM
#undef LIBX11_SYSTEM

/* Tell x11term.c and keyboard.c we have the system V streams feature.  */
#define SYSV_STREAMS

/* This definition was suggested for next release.
   So give it a try.  */
#define HAVE_SOCKETS

#define bcopy(src,dst,n)	memcpy(dst,src,n)
#define bcmp(src,dst,n)		memcmp(src,dst,n)
#define bzero(s,n)		memset(s,0,n)

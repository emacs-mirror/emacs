/* Definitions file for GNU Emacs running on AT&T's System V Release 4
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* This file written by James Van Artsdalen of Dell Computer Corporation.
 * james@bigtex.cactus.org.
 */

/* Use the SysVr3 file for at least base configuration. */

#include "usg5-3.h"

#define USG5_4

/* We do have multiple jobs.  Handle ^Z. */

#undef NOMULTIPLEJOBS

/* If compiled by GNU C, we must have gnulib */

#ifdef __GNUC__
#define GNULIB /usr/local/lib/gcc-gnulib
#define LIBS_DEBUG
#else
#define GNULIB
#endif

#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o

#define LIB_STANDARD GNULIB -lsocket -lnsl -lelf -lc /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o

#define DATA_SEG_BITS 0x08000000

/* Use ptem.h to get structures related to windows.  */

#define NEED_PTEM_H

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
#undef SIGIO
#endif

/* libc has this stuff, but not utimes. */

#define HAVE_RENAME
#define HAVE_SELECT
#define HAVE_TIMEVAL
#define HAVE_CLOSEDIR
#define HAVE_GETTIMEOFDAY
#define HAVE_DUP2

#define USE_UTIME

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

/* The definition of this in usg5-3.h is not needed in 5.4.  */

#undef LIBX11_SYSTEM

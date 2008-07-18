/* Definitions file for GNU Emacs running on AT&T's System V Release 4
   Copyright (C) 1987, 1990, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007, 2008  Free Software Foundation, Inc.

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

/* This file written by James Van Artsdalen of Dell Computer Corporation.
 * james@bigtex.cactus.org.  Subsequently improved for Dell 2.2 by Eric
 * S. Raymond <esr@snark.thyrsus.com>.
 */

/* Use the SysVr3 file for at least base configuration. */

#define USG				/* System III, System V, etc */

#define USG5
#define USG5_4

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "usg-unix-v"

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/*
 * 	Define SYSV_SYSTEM_DIR to use the V.3 getdents/readir
 *	library functions.  Almost, but not quite the same as
 *	the 4.2 functions
 */
#define SYSV_SYSTEM_DIR

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is supposed to work now on system V release 2.  */

#define subprocesses

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

#define sigsetmask(mask)	/* Null expansion */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

/* On USG systems these have different names */
#ifndef HAVE_INDEX
#define index strchr
#endif /* ! defined (HAVE_INDEX) */
#ifndef HAVE_RINDEX
#define rindex strrchr
#endif /* ! defined (HAVE_RINDEX) */

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Use terminfo instead of termcap.  */

#define TERMINFO


/* The docs for system V/386 suggest v.3 has sigpause,
   so let's give it a try.  */
#define HAVE_SYSV_SIGPAUSE


/* If we're using the System V X port, BSD bstring functions will be handy */

#ifdef HAVE_X_WINDOWS
#define BSTRING
#endif /* HAVE_X_WINDOWS */

/* On USG systems signal handlers return void */

#define SIGTYPE void

#define ORDINARY_LINK

#define LIB_STANDARD

/* there are no -lg libraries on this system, and no libPW */

#define LIBS_DEBUG

/* Undump with ELF */

#undef COFF

#define UNEXEC unexelf.o

/* Get FIONREAD from <sys/filio.h>.  Get <sys/ttold.h> to get struct
 * tchars. But get <termio.h> first to make sure ttold.h doesn't
 * interfere.  And don't try to use SIGIO yet.
 */

#ifndef NOT_C_CODE
#include <sys/wait.h>
#endif

#ifdef emacs
#include <sys/filio.h>
#include <termio.h>
#include <sys/ttold.h>
#include <signal.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/termios.h>
#define BROKEN_SIGIO
#endif

/* Some SVr4s don't define NSIG in sys/signal.h for ANSI environments;
 * instead, there's a system variable _sys_nsig.  Unfortunately, we need the
 * constant to dimension an array.  So wire in the appropriate value here.
 */
#define NSIG_MINIMUM 32

/* We can support this */

#define CLASH_DETECTION

#define HAVE_PTYS
#define HAVE_TERMIOS

/* It is possible to receive SIGCHLD when there are no children
   waiting, because a previous waitsys(2) cleaned up the carcass of child
   without clearing the SIGCHLD pending info.  So, use a non-blocking
   wait3 instead, which maps to waitpid(2) in SysVr4. */

#define wait3(status, options, rusage) \
  waitpid ((pid_t) -1, (status), (options))
#define WRETCODE(w) (w >> 8)

/* TIOCGPGRP is broken in SysVr4, so we can't send signals to PTY
   subprocesses the usual way.  But TIOCSIGNAL does work for PTYs, and
   this is all we need.  */

#define TIOCSIGSEND TIOCSIGNAL

/* This change means that we don't loop through allocate_pty too many
   times in the (rare) event of a failure. */

#define FIRST_PTY_LETTER 'z'

/* This sets the name of the master side of the PTY. */

#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");

/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#define PTY_TTY_NAME_SPRINTF				\
  {							\
    char *ptsname (), *ptyname;				\
							\
    sighold (SIGCLD);					\
    if (grantpt (fd) == -1)				\
      { emacs_close (fd); return -1; }			\
    sigrelse (SIGCLD);					\
    if (unlockpt (fd) == -1)				\
      { emacs_close (fd); return -1; }			\
    if (!(ptyname = ptsname (fd)))			\
      { emacs_close (fd); return -1; }			\
    strncpy (pty_name, ptyname, sizeof (pty_name));	\
    pty_name[sizeof (pty_name) - 1] = 0;		\
  }

/* Push various streams modules onto a PTY channel. */

#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1)	\
    fatal ("ioctl I_PUSH ptem", errno);		\
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) \
    fatal ("ioctl I_PUSH ttcompat", errno);

/* This definition was suggested for next release.
   So give it a try.  */
#define HAVE_SOCKETS

/* Markus Weiand <weiand@khof.com> says this is needed for Motif on
   SINIX.  */
#define LIBS_SYSTEM -lgen

/* arch-tag: 1a0ed909-5faa-434b-b7c3-9d86c63d53a6
   (do not change this comment) */

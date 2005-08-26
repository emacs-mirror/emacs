/* Definitions file for GNU Emacs running on Silicon Graphics 3.5
   Copyright (C) 1987, 1999, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG
#define USG5
#define IRIS

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "irix"

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'a'

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* The file containing the kernel's symbol table is called /vmunix.  */

#define KERNEL_FILE "/vmunix"

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"


/* Special hacks needed to make Emacs run on this system.  */

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

#define sigsetmask(mask)	/* Null expansion */

/* The IRIS defines SIGIO in signal.h, but doesn't implement it. */
#define BROKEN_SIGIO

#define LIBS_MACHINE -lbsd -ldbm -lPW
#define C_SWITCH_MACHINE -I/usr/include/bsd

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

#define _setjmp setjmp
#define _longjmp longjmp

/* On USG systems these have different names */

#define index strchr
#define rindex strrchr

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.  */

/* #define static */

/* Compiler bug bites on many systems when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (int)((char *)(x) - (char*)0)

/* some errno.h's don't actually allocate the variable itself */

#define NEED_ERRNO

/* The symbol FIONREAD is defined, but the feature does not work.  */

#define BROKEN_FIONREAD

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
 	    sprintf (ptyname, "/dev/ttyq%d", minor (stb.st_rdev));

/* arch-tag: 41161051-477f-44be-bb2a-0eead3142157
   (do not change this comment) */

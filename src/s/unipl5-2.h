/* Definitions file for GNU Emacs running on UniSoft's UniPlus 5.2
   Copyright (C) 1985, 1986, 1999, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007  Free Software Foundation, Inc.

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

#define USG				/* System III, System V, etc */

#define USG5

#define UNIPLUS

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "unisoft-unix"

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptyp0  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define this macro if system defines a type `union wait'.  */

#define HAVE_UNION_WAIT

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

/* #define HAVE_SOCKETS */

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

#define NONSYSTEM_DIR_LIBRARY

/* Define this symbol if your system has the functions bcopy, etc. */

/* #define BSTRING */

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is generally OS dependent, and not supported
 under most USG systems.  It is supported under UniPlus
 System V Release 2. */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* Uniplus 5.2 supports long names in C */

/* #define SHORTNAMES */

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

#define index strchr
#define rindex strrchr

/* Compiler bug bites when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Special library needed for linking for Uniplus */

#define LIBS_SYSTEM -lnet

/* A system-specific loader switch is needed.  */

#define LD_SWITCH_SYSTEM -N -L/lib/libg /usr/lib/unshared.ld

/* arch-tag: d3f952a9-fef5-45d5-9c1e-c1c32bfef86c
   (do not change this comment) */

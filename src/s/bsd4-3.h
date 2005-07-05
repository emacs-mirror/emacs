/* Definitions file for GNU Emacs running on bsd 4.3
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

/* We give these symbols the numeric values found in <sys/param.h> to
   avoid warnings about redefined macros.  */
#ifndef BSD4_3
#define BSD4_3 1
#endif /* BSD4_3 */

#ifndef BSD_SYSTEM
#define BSD_SYSTEM 43
#endif /* BSD_SYSTEM */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "berkeley-unix"

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Do not use interrupt_input = 1 by default, because in 4.3
   we can make noninterrupt input work properly.  */

#undef INTERRUPT_INPUT

/* First pty name is /dev/ptyp0.  */

#define FIRST_PTY_LETTER 'p'

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define this macro if system defines a type `union wait'.  */

#define HAVE_UNION_WAIT

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

#define CLASH_DETECTION

/* The file containing the kernel's symbol table is called /vmunix.  */

#define KERNEL_FILE "/vmunix"

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Send signals to subprocesses by "typing" special chars at them.  */

#define SIGNALS_VIA_CHARACTERS

/* arch-tag: 0c367245-bde3-492e-9029-3ff6898beb95
   (do not change this comment) */

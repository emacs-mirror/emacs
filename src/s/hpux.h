/* Definitions file for GNU Emacs running on HPUX release 7.0.
   Based on AT&T System V.2.
   Copyright (C) 1985, 1986, 1999, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
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

#define HPUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "hpux"

/* `nomultiplejobs' should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).

 On hpux this depends on the precise kind of machine in use,
 so the m- file defines this symbol if appropriate.  */

/* Default is to set interrupt_input to 0: don't do input buffering within Emacs */

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */

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

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc.
 * s800 and later versions of s300 (s200) kernels have equivalents
 * of the BSTRING functions of BSD.  If your s200 kernel doesn't have
 * em comment out this section.
 */

#define BSTRING

/* subprocesses should be defined if you want to
 have code for asynchronous subprocesses
 (as used in M-x compile and M-x shell).
 This is generally OS dependent, and not supported
 under most USG systems.  */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Say we have the SYSV style of interprocess communication.  */

#define HAVE_SYSVIPC

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* Define SHORTNAMES if the C compiler can distinguish only
   short names.  It means that the stuff in ../shortnames
   must be run to convert the long names to short ones.

   Some USG systems support long names.
   If yours is one, DO NOT change this file!
   Do #undef SHORTNAMES in the m- file or in config.h.  */

/* #define SHORTNAMES */

/* The file containing the kernel's symbol table is called /hp-ux.  */

#define KERNEL_FILE "/hp-ux"

/* The symbol in the kernel where the load average is found
   depends on the cpu type, so we let the m- files define LDAV_SYMBOL.  */

/* Special hacks needed to make Emacs run on this system.  */

/*
 *	Make the sigsetmask function go away.  Don't know what the
 *	ramifications of this are, but doesn't seem possible to
 *	emulate it properly anyway at this point.
 */

/* HPUX has sigsetmask */
/* #define sigsetmask(mask)	/ * Null expansion * / */

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */

/* HP-UX has _setjmp and _longjmp */
/*
#define _setjmp setjmp
#define _longjmp longjmp
*/

/* Use the system provided termcap(3) library */
#define TERMINFO

/* The 48-bit versions are more winning for Emacs;
   the ordinary ones don't give even 32 bits.  */
#define random lrand48
#define srandom srand48

/* In hpux, the symbol SIGIO is defined, but the feature
   doesn't work in the way Emacs needs it to.  */

#define BROKEN_SIGIO

/* USG systems tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   Foil this.  Emacs carefully avoids static vars inside functions.
   http://lists.gnu.org/archive/html/emacs-devel/2007-09/msg00368.html
   As of at least HPUX 11.11 (2000), it seems this workaround is no
   longer needed.  Try uncommenting the following if you have problems
   on older versions.  */

/* This is unnecessary in HPUX versions 10.20, 11.0, 11.11, 11.23.  */

/* #define static  */

/* Define extra libraries to load.
   This should have -lBSD, but that library is said to make
   `signal' fail to work.  */

#ifdef HPUX_NET
#define LIBS_SYSTEM -ln
#else
#define LIBS_SYSTEM
#endif

/* Some additional system facilities exist.  */

#define HAVE_PERROR  /* Delete this line for version 6.  */

/* The following maps shared exec file to demand loaded exec.
   Don't do this as demand loaded exec is broken in hpux.  */

#if 0

/* Adjust a header field for the executable file about to be dumped.  */

#define ADJUST_EXEC_HEADER   \
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?  \
		 NEWMAGIC : ohdr.a_magic);

#endif

/* Baud-rate values in tty status have nonstandard meanings.  */

#define BAUD_CONVERT  \
{ 0, 50, 75, 110, 135, 150, 200, 300, 600, 900, 1200,  \
  1800, 2400, 3600, 4800, 7200, 9600, 19200, 38400 }

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
            sprintf (pty_name, "/dev/pty/tty%c%x", c, i);

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF \
	sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#ifndef HPUX8
/* It's possible that HPUX 7 has sys/wait.h but it does not work right.  */
#undef HAVE_SYS_WAIT_H
#endif

/* arch-tag: 75c1239f-fedb-4f64-8f86-7893e75699e8
   (do not change this comment) */

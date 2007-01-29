/* Definitions file for GNU Emacs running on RTU 3.0, ucb universe.
   Copyright (C) 1986, 1999, 2001, 2002, 2003, 2004,
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

#define BSD4_2
#define BSD_SYSTEM
#define RTU

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "rtu"

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.
   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvantages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe.
*/

#undef INTERRUPT_INPUT

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'z'	/* i.e. no PTY_LETTERs */

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

#define NONSYSTEM_DIR_LIBRARY

/* Define this symbol if your system has the functions bcopy, etc. */
/* The system library bcopy() is broken in RTU.  For one thing, it expects
   the length to be less than 64k. */
#undef BSTRING

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#undef COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#undef MAIL_USE_FLOCK

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#undef CLASH_DETECTION

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/* The "fsync" call on RTU versions 3.0 and 3.1 is badly broken!
   This hack below isn't the best solution, but without it this
   program will cause the whole system to hang!  !@#$#%$ Masscomp!  */

#define fsync(x) 0	/* "Comment out" fsync calls */

/* RTU has IPC instead of Unix-domain sockets.  */

#define HAVE_SYSVIPC

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
            sprintf (pty_name, "/dev/ttyp%x", i);

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF \
	sprintf (pty_name, "/dev/pty%x", i);

/* Process groups work in the traditional BSD manner.  */

#define BSD_PGRPS

/* arch-tag: 0908970b-1cc0-4d57-b866-61b9bd029a0d
   (do not change this comment) */

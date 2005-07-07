/* Header file for Harris CXUX.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* #define UNIPLUS */
#define USG5
#define USG
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
/* #define BSD4_2 */
/* #define BSD4_3 */
/* #define BSD_SYSTEM */
/* #define VMS */

#ifndef	_CX_UX
#define	_CX_UX 1
#endif

/* Define this symbol if you are running CX/UX 7.0 or later (7.0 introduced
 * support for ELF files, and while we still build emacs in COFF format, the
 * way it is linked is different for 7.0).
 */
/* #define USING_CX_UX_7 */

#ifdef USING_CX_UX_7
#define LINKER /usr/sde/coff/usr/bin/ld
#define LD_SWITCH_SYSTEM -L/usr/sde/coff/usr/lib -zzero_word
#define START_FILES pre-crt0.o /usr/sde/coff/usr/lib/crt0.o /usr/sde/coff/usr/lib/m88100.o
#else	/* !USING_CX_UX_7 */
#ifdef	_M88K
#define	START_FILES pre-crt0.o /lib/crt0.o
#else
#define	START_FILES cxux-crt0.o /lib/crt0.o
#endif
#endif	/* USING_CX_UX_7 */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "usg-unix-v"

#define C_SWITCH_SYSTEM -Xa

#define POSIX_SIGNALS

/* With POSIX signals, also need to use sigaction rather than signal to
 * setup signal handlers
 */
#define signal sys_signal

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.

   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   Emacs uses the presence or absence of the SIGIO macro to indicate
   whether or not signal-driven I/O is possible.  It uses
   INTERRUPT_INPUT to decide whether to use it by default.

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvantages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe. */

#define INTERRUPT_INPUT
/* #define BROKEN_FIONREAD */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'A'
#define	PTY_ITERATION	for (c = 'A'; c <= 'P'; c++) for (i = 0; i < 16; i++)

/*
 *	Define HAVE_TERMIOS if the system provides POSIX-style
 *	functions and macros for terminal control.
 *
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 *
 *	Do not define both.  HAVE_TERMIOS is preferred, if it is
 *	supported on your system.
 */

#define HAVE_TERMIOS
/* #define HAVE_TERMIO */
#define NO_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

#define SYSV_SYSTEM_DIR

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

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* Define this if your operating system declares signal handlers to
   have a type other than the usual.  `The usual' is `void' for ANSI C
   systems (i.e. when the __STDC__ macro is defined), and `int' for
   pre-ANSI systems.  If you're using GCC on an older system, __STDC__
   will be defined, but the system's include files will still say that
   signal returns int or whatever; in situations like that, define
   this to be what the system's include files want.  */
/* #define SIGTYPE int */
#define SIGTYPE void

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
/* #define SEPCHAR ':' */

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Yes! The Night Hawk has sockets! */

#define HAVE_SOCKETS

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

#define KERNEL_FILE "/unix"

/* There are too many kludges required to redefine malloc - use the system
   one */
#define SYSTEM_MALLOC

#define _setjmp setjmp
#define _longjmp longjmp

/* const really does work, but I can't get configure to run the C compiler
 * with the right options so it figures that out.
 */
#undef const

#ifdef sigmask
#undef sigmask
#endif

/*
 * <pwd.h> already declares getpwuid, and with a uid_t argument in ANSI C
 * mode.  Define this so xrdb.c will compile
 */
#ifdef	__STDC__
#define	DECLARE_GETPWUID_WITH_UID_T
#endif

/* Some compilers tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   On these systems, you must #define static as nothing to foil this.
   Note that emacs carefully avoids static vars inside functions.  */

/* #define static */

/* arch-tag: 5febe5fe-f0b0-49cb-9280-9d5a9fa43710
   (do not change this comment) */

/* System description header file for Darwin (Mac OS X).
   Copyright (C) 2001 Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
/* #define USG */
/* #define HPUX */
/* #define UMAX */
/* #define BSD4_1 */
#define BSD4_2
/* BSD4_3 and BSD4_4 are already defined in sys/param.h */
/* #define BSD4_3 */
/* #define BSD4_4 */
#define BSD_SYSTEM
/* #define VMS */

/* MAC_OS is used to conditionally compile code common to both MAC_OS8
   and MAC_OSX.  */
#ifdef MAC_OSX
#define MAC_OS
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "darwin"

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

/* #define NOMULTIPLEJOBS */

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.

   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   Emacs uses the presence or absence of the SIGIO and BROKEN_SIGIO macros
   to indicate whether or not signal-driven I/O is possible.  It uses
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

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'p'

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

/* Define this if your operating system declares signal handlers to
   have a type other than the usual.  `The usual' is `void' for ANSI C
   systems (i.e. when the __STDC__ macro is defined), and `int' for
   pre-ANSI systems.  If you're using GCC on an older system, __STDC__
   will be defined, but the system's include files will still say that
   signal returns int or whatever; in situations like that, define
   this to be what the system's include files want.  */
/* #define SIGTYPE int */

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
/* #define SEPCHAR ':' */

/* Define this if the system can use mmap for buffer text allocation.  */
/* #define USE_MMAP_FOR_BUFFERS 1 */

/* ============================================================ */

/* Here, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* Some compilers tend to put everything declared static
   into the initialized data area, which becomes pure after dumping Emacs.
   On these systems, you must #define static as nothing to foil this.
   Note that emacs carefully avoids static vars inside functions.  */

/* #define static */

/* If the system's imake configuration file defines `NeedWidePrototypes'
   as `NO', we must define NARROWPROTO manually.  Such a define is
   generated in the Makefile generated by `xmkmf'.  If we don't 
   define NARROWPROTO, we will see the wrong function prototypes
   for X functions taking float or double parameters.  */

/*  #define NARROWPROTO 1 */

/* ============================================================ */

/* After adding support for a new system, modify the large case
   statement in the `configure' script to recognize reasonable
   configuration names, and add a description of the system to
   `etc/MACHINES'.

   If you've just fixed a problem in an existing configuration file,
   you should also check `etc/MACHINES' to make sure its descriptions
   of known problems in that configuration should be updated.  */


/* Avoid the use of the name init_process (process.c) because it is
   also the name of a Mach system call.  */
#define init_process emacs_init_process

/* Fix compilation problem for regex.c.  */
#define __restrict

/* Fix compilation problem for md5.c.  */
#define __attribute__(x)

/* Used in dispnew.c.  Copied from freebsd.h. */
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* Darwin provides POSIX-style getpgrp.  */
#define GETPGRP_NO_ARG

/* System uses OXTABS instead of the expected TAB3.  (Copied from
   bsd386.h.)  */
#define TAB3 OXTABS

/* Darwin ld insists on the use of malloc routines in the System
   framework.  */
#define SYSTEM_MALLOC

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* Extra initialization calls in main for Mac OS X system type.  */
#define SYMS_SYSTEM syms_of_mac()

/* Definitions for how to dump.  Copied from nextstep.h.  */

#define UNEXEC unexmacosx.o

#define START_FILES pre-crt0.o

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START (0)

/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext()

/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata()

/* Definitions for how to compile & link.  */

/* Indicate that we are compiling for Mac OS X and where to find Mac
   specific headers.  */
#define C_SWITCH_SYSTEM -fpascal-strings -fno-common -DMAC_OSX -I../mac/src

/* Link in the Carbon lib.  The -headerpad option tells ld (see man
   page) to leave room at the end of the header for adding load
   commands.  Needed for dumping.  0x690 is the total size of 30
   segment load commands (at 56 each).  */
#define LD_SWITCH_SYSTEM_TEMACS -prebind -framework Carbon -lstdc++ -Xlinker -headerpad -Xlinker 690

#define C_SWITCH_SYSTEM_TEMACS -Dtemacs

/* Link this program just by running cc.  */
#define ORDINARY_LINK

/* We don't have a g library, so override the -lg LIBS_DEBUG switch.  */
#define LIBS_DEBUG

/* Adding -lm confuses the dynamic linker, so omit it.  */
#define LIB_MATH

/* Tell src/Makefile.in to create files in the Mac OS X application
   bundle mac/Emacs.app.  */
#define OTHER_FILES macosx-app


/* Define the following so emacs symbols will not conflict with those
   in the System framework.  Otherwise -prebind will not work.  */

/* Do not define abort in emacs.c.  */
#define NO_ABORT

/* Do not define matherr in floatfns.c.  */
#define NO_MATHERR


/* This prevents a compilation error in xfaces.c: struct kboard * is
   used in a function protocol the first time this type appears in the
   file, since MULTI_KBOARD is undefined for the Mac OS X build.  */
#ifndef NOT_C_CODE
struct kboard;
#endif

#ifdef temacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
#endif

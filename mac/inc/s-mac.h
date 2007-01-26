/* Handcrafted s-mac.h file for building GNU Emacs on Mac OS 9.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
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

/* Contributed by Andrew Choi (akochoi@mac.com).  */

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
/* #define BSD4_2 */
/* #define BSD4_3 */
/* #define BSD_SYSTEM */
/* #define VMS */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "macos"

/* NOMULTIPLEJOBS should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

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

/* #define INTERRUPT_INPUT */

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

/* #define FIRST_PTY_LETTER 'a' */

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

/* #define HAVE_TERMIOS */
#define HAVE_TERMIO

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */

/* Define this symbol if your system has the functions bcopy, etc. */

/* #define BSTRING */

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

/* #define subprocesses */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

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

#ifdef __MRC__
/* MrC predefines signal numbers as powers of 2.  */
#define sigmask(no) (((no) & (no) - 1) ? 1L << ((no) - 1) : (no))
#define __signal_max 8	      /* There's enough room for the following
				 signals between 8 and 16, and the
				 maximum predefined one (32) is less
				 than 8th power of 2.  */
#endif

#define SIGHUP (__signal_max+1)
#define SIGQUIT (__signal_max+2)
#define SIGTRAP (__signal_max+3)
#define SIGKILL (__signal_max+4)
#define SIGALRM (__signal_max+5)
#define SIGPIPE (__signal_max+6)

#ifdef __MRC__
#define NSIG SIGTERM  /* largest one in signal.h */
#else
#define NSIG (__signal_max+6)
#endif

#ifdef __MRC__
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#elif __MWERKS__
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->buffer_ptr - (FILE)->buffer)
#endif

#ifdef __MWERKS__
#include <errno.h>
#ifndef ENOENT
#define ENOENT 100
#endif
#ifndef EXDEV
#define EXDEV 101
#endif
#ifndef EEXIST
#define EEXIST 102
#endif
#ifndef EINTR
#define EINTR 102
#endif
#ifndef EACCES
#define EACCES 103
#endif
#ifndef ENOTDIR
#define ENOTDIR 104
#endif
#ifndef EIO
#define EIO 105
#endif
#ifndef EBADF
#define EBADF 106
#endif
#endif

#define SYSTEM_PURESIZE_EXTRA (200*1024)

/* don't know what this will do, but sysdep.c needs it */
#define DATA_START 0

/* Limited by CW's 32K limit on local data! */
#define READ_BUF_SIZE (8 << 10)

#include <utsname.h>

#define SYSV_SYSTEM_DIR

#define SYSTEM_MALLOC

#define _setjmp setjmp
#define _longjmp longjmp

#define _exit exit

#define main emacs_main

/* Include this here so it won't be include again when #include in emacs
   sources.  Then undefine the macro definitions in it for unlink, read,
   write, access, and rmdir.  */
#ifdef __MWERKS__
#include <unistd.h>
#endif

#ifndef X_OK
#define X_OK 01
#endif

#undef unlink
#define unlink sys_unlink
#undef read
#define read sys_read
#undef write
#define write sys_write
#undef access
#define access sys_access
#undef rmdir
#define rmdir sys_rmdir

#define open sys_open
#define creat sys_creat

#define rename sys_rename
#define fopen sys_fopen
#define signal sys_signal

#define gmtime sys_gmtime
#define localtime sys_localtime
#define ctime sys_ctime
#define time sys_time

#define index strchr
#define rindex strrchr

#define PTR POINTER_TYPE *	/* For strftime.c.  */

#define SYMS_SYSTEM syms_of_mac()

#ifdef USE_LSB_TAG
#ifdef __MRC__
#define DECL_ALIGN(type, var) type var
#endif
#endif

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method for marking the
   stack.  */
#define GC_MARK_STACK   GC_MAKE_GCPROS_NOOPS

/* arch-tag: 6a941c4b-a419-4d25-80ac-9335053e58b2
   (do not change this comment) */

/* This file is the configuration file for Linux-based GNU systems
   Copyright (C) 1985, 1986, 1992, 1994, 1996, 1999, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

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

/* This file was put together by Michael K. Johnson and Rik Faith.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

/* #define UNIPLUS */
/* #define USG5 */
#define USG
/* #define BSD_SYSTEM */
#define LINUX
#define GNU_LINUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "gnu/linux"		/* All the best software is free. */

#ifndef NOT_C_CODE
#ifdef emacs
#ifdef HAVE_LINUX_VERSION_H
#include <linux/version.h>

#if LINUX_VERSION_CODE >= 0x20400
#define LINUX_SIGNALS_VIA_CHARACTERS_DOES_WORK
#endif /* LINUX_VERSION_CODE >= 0x20400 */
#endif /* HAVE_LINUX_VERSION_H */
#endif /* emacs */
#endif /* NOT_C_CODE */

#if defined HAVE_GRANTPT
#define UNIX98_PTYS

/* Run only once.  We need a `for'-loop because the code uses
   `continue'.  */

#define PTY_ITERATION	for (i = 0; i < 1; i++)

#ifdef HAVE_GETPT
#define PTY_NAME_SPRINTF
#define PTY_OPEN fd = getpt ()
#else /* not HAVE_GETPT */
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");
#endif /* not HAVE_GETPT */

/* Note that grantpt and unlockpt may fork.  We must block SIGCHLD to
   prevent sigchld_handler from intercepting the child's death.  */

#define PTY_TTY_NAME_SPRINTF				\
  {							\
    char *ptyname;					\
							\
    sigblock (sigmask (SIGCHLD));			\
    if (grantpt (fd) == -1 || unlockpt (fd) == -1	\
        || !(ptyname = ptsname(fd)))			\
      {							\
	sigunblock (sigmask (SIGCHLD));			\
	close (fd);					\
	return -1;					\
      }							\
    strncpy (pty_name, ptyname, sizeof (pty_name));	\
    pty_name[sizeof (pty_name) - 1] = 0;		\
    sigunblock (sigmask (SIGCHLD));			\
  }

#else /* not HAVE_GRANTPT */

/* Letter to use in finding device name of first pty,
   if system supports pty's.  'p' means it is /dev/ptyp0  */

#define FIRST_PTY_LETTER 'p'

#endif  /* not HAVE_GRANDPT */

/*  Define HAVE_TERMIOS if the system provides POSIX-style
    functions and macros for terminal control.  */

#define HAVE_TERMIOS

/* Define HAVE_PTYS if the system supports pty devices. */

#define HAVE_PTYS

#define HAVE_SOCKETS

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* On GNU/Linux systems, both methods are used by various mail
   programs.  I assume that most people are using newer mailers that
   have heard of flock.  Change this if you need to. */
/* Debian contains a patch which says: ``On Debian/GNU/Linux systems,
   configure gets the right answers, and that means *NOT* using flock.
   Using flock is guaranteed to be the wrong thing. See Debian Policy
   for details.'' and then uses `#ifdef DEBIAN'.  Unfortunately the
   Debian maintainer hasn't provided a clean fix for Emacs.
   movemail.c will use `maillock' when MAILDIR, HAVE_LIBMAIL and
   HAVE_MAILLOCK_H are defined, so the following appears to be the
   correct logic.  -- fx */
/* We must check for HAVE_LIBLOCKFILE too, as movemail does.
   liblockfile is a Free Software replacement for libmail, used on
   Debian systems and elsewhere. -rfr */

#if !((defined (HAVE_LIBMAIL) || defined (HAVE_LIBLOCKFILE)) &&	\
      defined (HAVE_MAILLOCK_H))
#define MAIL_USE_FLOCK
#endif

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

#define CLASH_DETECTION

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* If you mount the proc file system somewhere other than /proc
   you will have to uncomment the following and make the proper
   changes */

/* #define LINUX_LDAV_FILE "/proc/loadavg" */

/* This is needed for dispnew.c:update_frame */

#ifdef emacs
#include <stdio.h>  /* Get the definition of _IO_STDIO_H.  */
#if defined(_IO_STDIO_H) || defined(_STDIO_USES_IOSTREAM)
/* new C libio names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#else /* !_IO_STDIO_H */
/* old C++ iostream names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_pptr - (FILE)->_pbase)
#endif /* !_IO_STDIO_H */
#endif /* emacs */

/* Ask GCC where to find libgcc.a.  */
#define LIB_GCC `$(CC) $(C_SWITCH_X_SITE) -print-libgcc-file-name`

#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  */

#ifdef __mips__
#define LD_SWITCH_SYSTEM -G 0 LD_SWITCH_X_SITE_AUX
#else
#define LD_SWITCH_SYSTEM LD_SWITCH_X_SITE_AUX
#endif /* __mips__ */

/* Link temacs with -z nocombreloc so that unexec works right, whether or
   not -z combreloc is the default.  GNU ld ignores unknown -z KEYWORD
   switches, so this also works with older versions that don't implement
   -z combreloc.  */
#define LD_SWITCH_SYSTEM_TEMACS -z nocombreloc

#ifdef emacs
#define INTERRUPT_INPUT
#endif

/* This is needed for sysdep.c */

#define NO_SIOCTL_H           /* don't have sioctl.h */

#define HAVE_WAIT_HEADER

#define SYSV_SYSTEM_DIR       /* use dirent.h */

#define POSIX                 /* affects getpagesize.h and systty.h */
#define POSIX_SIGNALS

/* Best not to include -lg, unless it is last on the command line */
#define LIBS_DEBUG
#undef LIB_GCC
#define LIB_GCC
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtn.o

/* Don't use -g in test compiles in configure.
   This is so we will use the same shared libs for that linking
   that are used when linking temacs.  */
#ifdef THIS_IS_CONFIGURE
#define C_DEBUG_SWITCH
#endif

/* 21 Jun 06: Eric Hanchrow <offby1@blarg.net> says this works.  */
#ifdef LINUX_SIGNALS_VIA_CHARACTERS_DOES_WORK
#define SIGNALS_VIA_CHARACTERS
#endif

/* Rob Malouf <malouf@csli.stanford.edu> says:
   SYSV IPC is standard a standard part of Linux since version 0.99pl10,
   and is a very common addition to previous versions.  */

#ifdef TERM
#define LIBS_SYSTEM -lclient
#define C_SWITCH_SYSTEM -D_BSD_SOURCE -I/usr/src/term
#else
/* alane@wozzle.linet.org says that -lipc is not a separate library,
   since libc-4.4.1.  So -lipc was deleted.  */
#define LIBS_SYSTEM
/* _BSD_SOURCE is redundant, at least in glibc2, since we define
   _GNU_SOURCE.  Left in in case it's relevant to libc5 systems and
   anyone's still using Emacs on those.  --fx 2002-12-14  */
#define C_SWITCH_SYSTEM -D_BSD_SOURCE
#endif

/* Paul Abrahams <abrahams@equinox.shaysnet.com> says this is needed.  */
#define LIB_MOTIF -lXm -lXpm

#ifdef HAVE_LIBNCURSES
#define TERMINFO
#define LIBS_TERMCAP -lncurses
#endif

#define HAVE_SYSVIPC

#define UNEXEC unexelf.o

#define A_TEXT_OFFSET(hdr) (N_MAGIC(hdr) == QMAGIC ? sizeof (struct exec) : 0)
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#define ADJUST_EXEC_HEADER \
  unexec_text_start = N_TXTADDR(ohdr) + A_TEXT_OFFSET(ohdr)

/* This is to work around mysterious gcc failures in some system versions.
   It is unlikely that Emacs changes will work around this problem;
   therefore, this should remain permanently.  */
#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

/* Use BSD process groups, but use setpgid() instead of setpgrp() to
   actually set a process group. */

#define BSD_PGRPS

#define NARROWPROTO 1

/* Use mmap directly for allocating larger buffers.  */
#ifdef DOUG_LEA_MALLOC
#undef REL_ALLOC
#endif

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the
   jmp_buf.  */
/* Not all the architectures are tested, but there are Debian packages
   for SCM and/or Guile on them, so the technique must work.  See also
   comments in alloc.c concerning setjmp and gcc.  Fixme:  it's
   probably safe to make this conditional just on GCC, except for ia64
   register window-flushing.  */
/* Don't use #cpu here since in newest development versions of GCC,
   we must call cpp with -traditional, and that disables #cpu.  */

#if defined __i386__ || defined __sparc__ || defined __mc68000__ \
    || defined __alpha__ || defined __mips__ || defined __s390__ \
    || defined __arm__ || defined __powerpc__ || defined __amd64__ \
    || defined __ia64__
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
#ifdef __mc68000__
#define GC_LISP_OBJECT_ALIGNMENT 2
#endif
#ifdef __ia64__
#define GC_MARK_SECONDARY_STACK()				\
  do {								\
    extern void *__libc_ia64_register_backing_store_base;	\
    __builtin_ia64_flushrs ();					\
    mark_memory (__libc_ia64_register_backing_store_base,	\
		 __builtin_ia64_bsp (), 0);			\
  } while (0)
#endif
#endif

/* arch-tag: 6244ea2a-abd0-44ec-abec-ff3dcc9afea9
   (do not change this comment) */

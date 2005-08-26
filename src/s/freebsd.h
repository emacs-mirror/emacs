/* System description header for FreeBSD systems.
   This file describes the parameters that system description files
   should define or not.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
                 2003, 2004, 2005 Free Software Foundation, Inc.

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

/* Get the correct __FreeBSD_version, even if this is before that was
   defined. */
#ifndef __FreeBSD_version
#ifndef __FreeBSD__
#define __FreeBSD_version 199401
#elif __FreeBSD__ == 1
#define __FreeBSD_version 199405
#else
#include <osreldate.h>
#endif
#endif /* !defined __FreeBSD_version */

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.
   You can inhibit them on newer systems if you wish
   by defining NO_SHARED_LIBS.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif


#if 0 /* This much, alone, seemed sufficient as of 19.23.
	 But it seems better to be independent of netbsd.h.  */
#include "netbsd.h"

#undef LIB_GCC
#define LIB_GCC -lgcc
#undef NEED_ERRNO
#endif /* 0 */


/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h. */
#define BSD4_2

/* These aren't needed, since we have getloadavg.  */
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil
#if __FreeBSD_version < 400000
#define LIBS_TERMCAP -ltermcap
#else
#define TERMINFO
#define LIBS_TERMCAP -lncurses
#endif

#define SYSV_SYSTEM_DIR

/* freebsd has POSIX-style pgrp behavior. */
#undef BSD_PGRPS
#define GETPGRP_NO_ARG

#ifdef __ELF__

#define LD_SWITCH_SYSTEM_1
#define START_FILES pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtbegin.o
#define UNEXEC unexelf.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o /usr/lib/crtn.o
#undef LIB_GCC
#define LIB_GCC

#else /* not __ELF__ */

#ifndef NO_SHARED_LIBS
#define LD_SWITCH_SYSTEM_1 -e start -dc -L/usr/local/lib
#define HAVE_TEXT_START		/* No need to define `start_of_text'. */
#if __FreeBSD_version >= 300002
#define START_FILES pre-crt0.o /usr/lib/aout/crt0.o
#else /* __FreeBSD_version < 300002 */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#endif /* __FreeBSD_version < 300002 */
#define UNEXEC unexsunos4.o
#define RUN_TIME_REMAP
#define LIB_GCC -lgcc

#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif
#else /* NO_SHARED_LIBS */
#ifdef __FreeBSD__  /* shared libs are available, but the user prefers
                     not to use them.  */
#define LD_SWITCH_SYSTEM_1 -Bstatic -L/usr/local/lib
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#endif /* __FreeBSD__ */
#endif /* NO_SHARED_LIBS */

#endif /* not __ELF__ */

/* Let `ld' find image libs and similar things in /usr/local/lib.  The
   system compiler, GCC, has apparently been modified to not look
   there, contrary to what a stock GCC would do.  */

#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_1 -L/usr/local/lib

#define HAVE_WAIT_HEADER
#define HAVE_GETLOADAVG 1
#if 0
#define HAVE_GETPAGESIZE  /* configure now puts this in config.h */
#endif
#define HAVE_TERMIOS
#define NO_TERMIO
#define DECLARE_GETPWUID_WITH_UID_T

/* freebsd uses OXTABS instead of the expected TAB3. */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* this silences a few compilation warnings */
#undef BSD_SYSTEM
#if __FreeBSD__ == 1
#define BSD_SYSTEM 199103
#elif __FreeBSD__ == 2
#define BSD_SYSTEM 199306
#elif __FreeBSD__ >= 3
#define BSD_SYSTEM 199506
#endif

#if 0  /* Shouldn't be necessary and produces warnings with the
          experimental Autoconf test.  */
#define WAITTYPE int
/* get this since it won't be included if WAITTYPE is defined */
#ifdef emacs
#include <sys/wait.h>
#endif
#define WRETCODE(w) (_W_INT(w) >> 8)
#endif

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

/* CLASH_DETECTION is defined in bsd4-3.h.
   In FreeBSD 2.1.5 (and other 2.1.x), this results useless symbolic links
   remaining in /tmp or other directories with +t bit.
   To avoid this problem, you could #undef it to use no file lock. */
/* #undef CLASH_DETECTION */

/* If the system's imake configuration file defines `NeedWidePrototypes'
   as `NO', we must define NARROWPROTO manually.  Such a define is
   generated in the Makefile generated by `xmkmf'.  If we don't
   define NARROWPROTO, we will see the wrong function prototypes
   for X functions taking float or double parameters.  */

#define NARROWPROTO 1

/* The following is needed to make `configure' find Xpm, Xaw3d and
   image include and library files if using /usr/bin/gcc.  That
   compiler seems to be modified to not find headers in
   /usr/local/include or libs in /usr/local/lib by default.  */

#define C_SWITCH_SYSTEM -I/usr/X11R6/include -I/usr/local/include -L/usr/local/lib

/* Circumvent a bug in FreeBSD.  In the following sequence of
   writes/reads on a PTY, read(2) returns bogus data:

   write(2)  1022 bytes
   write(2)   954 bytes, get EAGAIN
   read(2)   1024 bytes in process_read_output
   read(2)     11 bytes in process_read_output

   That is, read(2) returns more bytes than have ever been written
   successfully.  The 1033 bytes read are the 1022 bytes written
   successfully after processing (for example with CRs added if the
   terminal is set up that way which it is here).  The same bytes will
   be seen again in a later read(2), without the CRs.  */

#define BROKEN_PTY_READ_AFTER_EAGAIN 1

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the
   jmp_buf.  */

#define GC_SETJMP_WORKS 1

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method for marking the
   stack.  */

#define GC_MARK_STACK 	GC_MAKE_GCPROS_NOOPS

/* Define USE_MMAP_FOR_BUFFERS to let Emacs use mmap(2) to allocate
   buffer text.  This overrides REL_ALLOC.  */

#define USE_MMAP_FOR_BUFFERS	1

/* Use sigprocmask(2) and friends instead of sigblock(2); the man page
   of sigblock says it is obsolete.  */

#define POSIX_SIGNALS		1

/* The `combreloc' setting became the default, and it seems to be
   incompatible with unexec.  Symptom is an immediate SEGV in
   XtInitializeWidget when starting Emacs under X11.  */

#if defined __FreeBSD_version && __FreeBSD_version >= 500042
#define LD_SWITCH_SYSTEM_TEMACS -znocombreloc
#endif

/* arch-tag: 426529ca-b7c4-448f-b10a-d4dcdc9c78eb
   (do not change this comment) */

/*
Copyright (C) 1999, 2001-2012  Free Software Foundation, Inc.

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

/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */
#define USG				/* System III, System V, etc */
#define USG5

/* This symbol should be defined on AIX Version 3  ??????? */
#ifndef _AIX
#define _AIX
#endif

/* In AIX, you allocate a pty by opening /dev/ptc to get the master side.
   To get the name of the slave side, you just ttyname() the master side.  */
#define PTY_ITERATION int c; for (c = 0; !c ; c++)
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptc");
#define PTY_TTY_NAME_SPRINTF strcpy (pty_name, ttyname (fd));

/* Define HAVE_PTYS if the system supports pty devices.  */
#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* Special items needed to make Emacs run on this system.  */

/* AIX doesn't define this.  */
#define unix 1

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#define SIGNALS_VIA_CHARACTERS
#define CLASH_DETECTION

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#undef sigmask

#ifndef HAVE_LIBXMU
/* Unfortunately without libXmu we cannot support EditRes.  */
#define NO_EDITRES
#endif

/* On AIX Emacs uses the gmalloc.c malloc implementation.  But given
   the way this system works, libc functions that return malloced
   memory use the libc malloc implementation. Calling xfree or
   xrealloc on the results of such functions results in a crash.

   One solution for this could be to define SYSTEM_MALLOC in configure,
   but that does not currently work on this system.

   It is possible to completely override the malloc implementation on
   AIX, but that involves putting the malloc functions in a shared
   library and setting the MALLOCTYPE environment variable to point to
   that shared library.

   Emacs currently calls xrealloc on the results of get_current_dir name,
   to avoid a crash just use the Emacs implementation for that function.  */
#define BROKEN_GET_CURRENT_DIR_NAME 1

/*** BUILD 9008 - FIONREAD problem still exists in X-Windows. ***/
#define BROKEN_FIONREAD
/* As we define BROKEN_FIONREAD, SIGIO will be undefined in systty.h.
   But, on AIX, SIGAIO, SIGPTY, and SIGPOLL are defined as SIGIO,
   which causes compilation error at init_signals in sysdep.c.  So, we
   define these macros so that syssignal.h detects them and undefine
   SIGAIO, SIGPTY and SIGPOLL.  */
#define BROKEN_SIGAIO
#define BROKEN_SIGPTY
#define BROKEN_SIGPOLL

/* Conservative garbage collection has not been tested, so for now
   play it safe and stick with the old-fashioned way of marking.  */
#define GC_MARK_STACK GC_USE_GCPROS_AS_BEFORE

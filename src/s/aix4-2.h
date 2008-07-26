/*
Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
  Free Software Foundation, Inc.

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

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG				/* System III, System V, etc */
#define USG5

/*      This symbol should be defined on AIX Version 3  ??????? */
#ifndef _AIX
#define _AIX
#endif

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "aix"

/* In AIX, you allocate a pty by opening /dev/ptc to get the master side.
   To get the name of the slave side, you just ttyname() the master side.  */

#define PTY_ITERATION for (c = 0; !c ; c++)
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptc");
#define PTY_TTY_NAME_SPRINTF strcpy (pty_name, ttyname (fd));

/*
 *	Define HAVE_TERMIO if the system provides sysV-style ioctls
 *	for terminal control.
 */

#define HAVE_TERMIOS

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

#define HAVE_SOCKETS


/*
 * 	Define SYSV_SYSTEM_DIR to use the V.3 getdents/readir
 *	library functions.  Almost, but not quite the same as
 *	the 4.2 functions
 */

#define SYSV_SYSTEM_DIR

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special itemss needed to make Emacs run on this system.  */

#ifndef __GNUC__
#define LINKER cc
#endif

/* Prevent -lg from being used for debugging.  Not needed.  */

#define LIBS_DEBUG

/* No need to specify -lc when linking.  */

#define LIB_STANDARD

/* Use terminfo instead of termcap.  */

#define TERMINFO

/* The following definition seems to be needed in AIX version 3.1.6.8.
   It may not have been needed in certain earlier versions.  */
#define HAVE_TCATTR

/* Include unistd.h, even though we don't define POSIX.  */
#define NEED_UNISTD_H

/* AIX doesn't define this.  */
#define unix 1

#ifndef __GNUC__
/* Some programs in src produce warnings saying certain subprograms
   are to comples and need a MAXMEM value greater than 2000 for
   additional optimization.  --nils@exp-math.uni-essen.de */
#define C_SWITCH_SYSTEM -ma -qmaxmem=4000
#endif

/* string.h defines rindex as a macro, at least with native cc, so we
   lose declaring char * rindex without this.
   It is just a guess which versions of AIX need this definition.  */
#undef HAVE_STRING_H

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
/* The above isn't generally true.  If it occurs with some compiler
   release, seek a fixed version, be it XLC or GCC.  The XLC version
   isn't tied to the OS version on AIX any more than elsewhere.  XLC
   (the IBM compiler) can use -g with -O.  (-O3 is also a possibility
   for the optimization level.)  -- fx, after David Edelsohn.  */
#define C_DEBUG_SWITCH -g -O

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#define SIGNALS_VIA_CHARACTERS
#define MAIL_USE_LOCKF
#define CLASH_DETECTION

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#define POSIX_SIGNALS
#undef sigmask

/* olson@mcs.anl.gov says -li18n is needed by -lXm.  */
#define LIB_MOTIF -lXm -li18n

#ifndef HAVE_LIBXMU
#define LIBXMU

/* Unfortunately without libXmu we cannot support EditRes.  */
#define NO_EDITRES
#endif

/* On AIX Emacs uses the gmalloc.c malloc implementation.  But given
   the way this system works, libc functions that return malloced
   memory use the libc malloc implementation. Calling xfree or
   xrealloc on the results of such functions results in a crash. 

   One solution for this could be to define SYSTEM_MALLOC here, but
   that does not currently work on this system.

   It is possible to completely override the malloc implementation on
   AIX, but that involves putting the malloc functions in a shared
   library and setting the MALLOCTYPE environment variable to point to
   tha shared library.
   
   Emacs currently calls xrealloc on the results of get_current_dir name,
   to avoid a crash just use the Emacs implementation for that function.
*/
#define BROKEN_GET_CURRENT_DIR_NAME 1

/* arch-tag: 38fe75ea-6aef-42bd-8449-bc34d921a562
   (do not change this comment) */

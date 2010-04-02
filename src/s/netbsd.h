/* s/ file for netbsd system.

   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008, 2009, 2010  Free Software Foundation, Inc.

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


/* Get most of the stuff from bsd-common */
#include "bsd-common.h"

#define HAVE_GETLOADAVG 1

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define LIBS_TERMCAP -ltermcap

#define START_FILES pre-crt0.o /usr/lib/crt0.o START_FILES_1 /usr/lib/crtbegin.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o END_FILES_1
#undef LIB_GCC
#define LIB_GCC

#ifdef HAVE_CRTIN
#define START_FILES_1 /usr/lib/crti.o 
#define END_FILES_1 /usr/lib/crtn.o
#else
#define START_FILES_1
#define END_FILES_1
#endif

#define AMPERSAND_FULL_NAME

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  We convert it to a -rpath option
   which is what OSF1 uses.  */
#define LD_SWITCH_SYSTEM_tmp `echo LD_SWITCH_X_SITE_AUX | sed -e 's/-R/-Wl,-rpath,/'`
#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp -Wl,-rpath,/usr/pkg/lib -L/usr/pkg/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib

/* The following is needed to make `configure' find Xpm, Xaw3d and
   image include and library files if using /usr/bin/gcc.  That
   compiler seems to be modified to not find headers in
   /usr/local/include or libs in /usr/local/lib by default.  */

#define C_SWITCH_SYSTEM -I/usr/X11R6/include -I/usr/pkg/include -I/usr/local/include -L/usr/pkg/lib -L/usr/local/lib

/* Link temacs with -z nocombreloc so that unexec works right, whether or
   not -z combreloc is the default.  GNU ld ignores unknown -z KEYWORD
   switches, so this also works with older versions that don't implement
   -z combreloc.  */

#define LD_SWITCH_SYSTEM_TEMACS -Wl,-z,nocombreloc

/* On post 1.3 releases of NetBSD, gcc -nostdlib also clears
   the library search parth, i.e. it won't search /usr/lib
   for libc and friends. Using -nostartfiles instead avoids
   this problem, and will also work on earlier NetBSD releases */

#define LINKER $(CC) -nostartfiles

#define DEFAULT_SOUND_DEVICE "/dev/audio"

/* Greg A. Woods <woods@weird.com> says we must include signal.h
   before syssignal.h is included, to work around interface conflicts
   that are handled with CPP __RENAME() macro in signal.h.  */

#ifndef NOT_C_CODE
#include <signal.h>
#endif

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */

#define DONT_REOPEN_PTY

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the
   jmp_buf.  */

#define GC_SETJMP_WORKS 1

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method.  */

#define GC_MARK_STACK	GC_MAKE_GCPROS_NOOPS

/* Use sigprocmask and friends instead of sigblock;
   sigblock is considered obsolete on NetBSD.  */

#define POSIX_SIGNALS	1

/* arch-tag: e80f364a-04e9-4faf-93cb-f36a0fe95c81
   (do not change this comment) */

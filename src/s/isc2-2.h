/* system description file for Interactive (ISC) Unix version 2.2 on the 386.

   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006,
                 2007  Free Software Foundation, Inc.

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


#include "usg5-3.h"

/* select (in -linet) works okay on X ptys, but not on the serial port.
   karl@cs.umb.edu says that with that select call, subprocesses made by
   (e.g.) M-x grep don't exit cleanly, they just hang.  Similar problems
   have been observed in ISC 3.0.  */
#define BROKEN_SELECT_NON_X

/* karl@cs.umb.edu says that ISC's socket support (in -linet) isn't
   what Emacs needs; it makes interrupt-shell-subjob and the like do
   nothing.  But that appears to have been another manifestation of
   the broken select, so it should now be safe to define this again.  */
#define HAVE_SOCKETS

#define NO_SOCKETS_IN_FILE_SYSTEM
#define NEED_NET_ERRNO_H

/* This keeps the .cdbx section that gcc puts out when generating
   stabs-in-coff output, so Emacs can be debugged.  --karl@cs.umb.edu. */
#define USG_SHARED_LIBRARIES

/* We can support lock files.  */
#define CLASH_DETECTION
#define NO_FCHMOD

#define HAVE_PTYS
#define MAXNAMLEN 512
#define O_NDELAY O_NONBLOCK
#define MEMORY_IN_STRING_H

/* Tell gmalloc.c that we don't have memmove (system include files to the
   contrary!). */
#define MEMMOVE_MISSING

/* Send a signal to a subprocess by "typing" a signal character. */
#define SIGNALS_VIA_CHARACTERS

/* -lPW is only needed if not using Gcc.  We used to include -lcposix here
   for the rename function, but some people say ISC's rename doesn't
   work correctly with Emacs so we use Emacs' emulation instead. */
#if defined (__GNUC__)
#  define LIB_STANDARD_1 -lcposix
#else /* !__GNUC__ */
#  define LIB_STANDARD_1 -lPW
#endif /* !__GNUC__ */

/* LIB_STANDARD_1 is used both here and in LIBS_SYSTEM
   (the latter for the sake of configure).  */
#define LIB_STANDARD LIB_STANDARD_1 -lc

#define NO_X_DESTROY_DATABASE

/* -linet may be needed to avoid undefined symbols such as gethostname,
   inet_addr, gethostbyname, socket, connect, ...  */
#define LIBS_SYSTEM -linet LIB_STANDARD_1

/* This system has job control.  */
#undef NOMULTIPLEJOBS

/* Inhibit asm code in netinet/in.h.  Strictly speaking, only necessary
   when -traditional is being used, but it doesn't hurt to
   unconditionally define this.  */
#define NO_ASM

/* -traditional is not necessary if the system header files are fixed to
   define getc and putc in the absence of _POSIX_SOURCE.  GCC's from 2.4.4
   on do this. */
#if !defined (__GNUC__) || __GNUC__ < 2
#  define C_SWITCH_SYSTEM -traditional
#endif

/* Some versions of ISC are said to define S_IFLNK even tho
   they don't really support symlinks.  */
#undef S_IFLNK

/* arch-tag: 4de02713-eac5-4360-9d36-fd82c7a3ae44
   (do not change this comment) */

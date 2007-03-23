/* Definitions file for GNU Emacs running on sunos 4.0.

   Copyright (C) 1994, 2001, 2002, 2003, 2004, 2005, 2006,
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


#include "bsd4-2.h"

#ifndef SUNOS4
#define SUNOS4
#endif

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

/* We use the Sun syntax -Bstatic unconditionally, because even when we
   use GCC, these are passed through to the linker, not handled by GCC
   directly.  */
#define LD_SWITCH_SYSTEM -Bstatic

/* We use this for linking temacs, but not for other programs
   or for tests in configure.  */
#define LD_SWITCH_SYSTEM_TEMACS -e __start

/* In SunOS 4.1, a static function called by tzsetwall reportedly
   clears the byte just past an eight byte region it mallocs, corrupting
   GNU malloc's memory pool.  But Sun's malloc doesn't seem to mind. */

#define SYSTEM_MALLOC

/* SunOS 4.x cc <stdlib.h> declares abort and free to return int.  */

#ifndef __STDC__
#define ABORT_RETURN_TYPE int
#define FREE_RETURN_TYPE int
#endif

#ifdef __GNUC__
/* We must define mkdir with this arg prototype
   to match GCC's fixed stat.h.  */
#define MKDIR_PROTOTYPE \
  int mkdir (const char *dpath, unsigned short dmode)
#endif /* __GNUC__ */

/* Must use the system's termcap, if we use any termcap.
   It does special things.  */

#ifndef TERMINFO
#define LIBS_TERMCAP -ltermcap
#endif

#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: 362f3bfc-810d-4f6e-9b83-5a32f8f1a926
   (do not change this comment) */

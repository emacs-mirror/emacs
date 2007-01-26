/* Definitions file for GNU Emacs running on Sony's NEWS-OS 5.0.2
   Copyright (C) 1992, 1994, 2001, 2002, 2003, 2004,
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

/* Use the SysVr4 file for at least base configuration. */

#include "usg5-4.h"

#define NEWSOS5

/* These will be defined by "m-mips.h". */
#undef START_FILES
#undef LIB_STANDARD

#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lgen

/* Disable use of "unexelf.c" and shared libraries, because
   "unexelf.c" doesn't work correctly on NEWS-OS.  "unexmips.c" does
   work correctly if the program is linked statically without ELF. */
#undef UNEXEC
#undef USG_SHARED_LIBRARIES

/* Use `ld' directly rather than ordinary link, because ordinary link
   can't produce a non-ELF executable.  */
#undef ORDINARY_LINK
#define LINKER /usr/lib/cmplrs/cc/ld
#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o
#define LIB_STANDARD -lc /usr/ccs/lib/crtn.o /usr/ccs/lib/values-Xt.o

#ifndef HAVE_SOCKETS
#define HAVE_SOCKETS
#endif

/* arch-tag: 2bb78fcd-fbc4-46dd-a14b-e4a9be957fe0
   (do not change this comment) */

/* Definitions file for GNU Emacs running on the GNU Hurd.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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


/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h.  */
#define BSD4_2

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

#undef NLIST_STRUCT

/* XXX should getloadavg be in libc?  Should we have a libutil?
#define HAVE_GETLOADAVG */
#undef NLIST_STRUCT
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define SIGNALS_VIA_CHARACTERS

#define HAVE_TERMIOS
#define NO_TERMIO

#define LIBS_DEBUG

/* XXX emacs should not expect TAB3 to be defined.  */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* Tell Emacs that we are a terminfo based system; disable the use
   of local termcap.  (GNU uses ncurses.) */
#ifdef HAVE_LIBNCURSES
#define TERMINFO
#define LIBS_TERMCAP -lncurses
#endif

#define SYSV_SYSTEM_DIR

/* GNU has POSIX-style pgrp behavior.  */
#undef BSD_PGRPS
#define GETPGRP_NO_ARG

/* Use mmap directly for allocating larger buffers.  */
#ifdef DOUG_LEA_MALLOC
#undef REL_ALLOC
#endif

#define HAVE_WAIT_HEADER
#define WAIT_USE_INT
#define HAVE_UNION_WAIT

/* GNU needs its own crt0, and libc defines data_start.  */
#define ORDINARY_LINK
#define DATA_START ({ extern int data_start; (char *) &data_start; })

/* GNU now always uses the ELF format.  */
#define UNEXEC unexelf.o

/* Some losing code fails to include this and then assumes
   that because it is braindead that O_RDONLY==0.  */
#ifndef NOT_C_CODE
#include <fcntl.h>
#endif

#define NARROWPROTO 1

#ifdef emacs
#include <stdio.h>  /* Get the definition of _IO_STDIO_H.  */
#if defined(_IO_STDIO_H) || defined(_STDIO_USES_IOSTREAM)
/* new C libio names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#endif /* !_IO_STDIO_H */
#endif /* emacs */

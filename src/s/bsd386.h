/* s/ file for bsd386 system.

   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

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


#include "bsd4-3.h"

#ifndef __bsdi__
#define __bsdi__ 1
#endif

#define DECLARE_GETPWUID_WITH_UID_T

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)
#define A_TEXT_OFFSET(x)    (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define LIBS_DEBUG
#define LIB_X11_LIB -L/usr/X11/lib -lX11
#define LIBS_SYSTEM -lutil -lkvm -lcompat

#define HAVE_GETLOADAVG 1

#undef BSD_PGRPS

/* System uses OXTABS instead of the expected TAB3.
   (Copied from netbsd.h.)  */
#define TABDLY OXTABS
#define TAB3 OXTABS

#define SYSV_SYSTEM_DIR

#define HAVE_TERMIOS
#define NO_TERMIO

#define WAITTYPE int
/* get this since it won't be included if WAITTYPE is defined */
#ifdef emacs
#include <sys/wait.h>
#endif
#define WRETCODE(w) WEXITSTATUS(w)
#ifndef WCOREDUMP
#define WCOREDUMP(w) ((w) & 0200)
#endif

#define GETPGRP_NO_ARG 1

/* arch-tag: 867e3bb8-e9df-4763-9c82-8f4accb8209e
   (do not change this comment) */

/* Definitions file for GNU Emacs running on Data General's DG/UX
   5.4 Release 2.xx systems.
   Copyright (C) 1994, 2001, 2002, 2003, 2004, 2005,
                 2006, 2007  Free Software Foundation, Inc.

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

#include "dgux.h"

/* There is a known kernel bug in DGUX 5.4R2.xx when using
   INTERRUPT_INPUT and invoking Emacs with a job control shell (csh,
   ksh, etc.) in the background.  This bug manifests itself by
   outputting `stop on tty output' and hanging.  The workaround is to
   set BROKEN_FIONREAD.
   -pmr@pajato.com */

#ifndef BROKEN_FIONREAD
#define BROKEN_FIONREAD
#endif
#ifdef INTERRUPT_INPUT
#undef INTERRUPT_INPUT
#endif

/* In DGUX 5.4R2.xx the function inet_addr() returns a `struct
   in_addr' instead of the more common `unsigned long'.
   -pmr@pajato.com */

#define HAVE_BROKEN_INET_ADDR

#if 0  /* Shawn M. Carey <smcarey@mailbox.syr.edu> found this
	  caused trouble on DGUX 5.4.2.  */
#define LIBS_SYSTEM -ldgc
#endif

/* arch-tag: a14f4043-6caa-4f01-a9b9-ae0fb0d2c96e
   (do not change this comment) */

/* Definitions file for GNU Emacs running on Data General's DG/UX
   version 5.4 Release 3.00 and above.
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

/* NOTE: DGUX5.4R3.00 will not build with the delivered gcc-2.4.5
   compiler.  You must upgraded to at least gcc-2.5.8.  If you are
   running DGUX 5.4R3.00 check on the system dg-rtp.dg.com:/pub/gnu
   for gcc-2.5.8 or later compiler.
   -pmr@pajato.com */

#include "dgux5-4r2.h"

/* DGUX 5.4R3.00 brought the definition of `struct inet_addr' into
   compliance with the majority of Unix systems.  The workaround
   introduced in 5.4R2 is no longer necessary. */

#ifdef HAVE_BROKEN_INET_ADDR
#undef HAVE_BROKEN_INET_ADDR
#endif

/* The `stop on tty output' problem which occurs when using
   INTERRUPT_INPUT and when Emacs is invoked under X11 using a job
   control shell (csh, ksh, etc.) in the background has not been fixed in
   DGUX 5.4R3.00.
   -pmr@pajato.com */

#if 0
#ifdef BROKEN_FIONREAD
#undef BROKEN_FIONREAD
#endif
#ifndef INTERRUPT_INPUT
#define INTERRUPT_INPUT
#endif
#endif

/* Under DGUX 5.4R3.00, getting a debuggable executable has been
   greatly simplified and applies to either COFF or ELF
   environments. */

#ifdef C_DEBUG_SWITCH
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g
#endif

/* This is needed according to Ehud Karni <ehud@unix.simonwiesel.co.il>
   for m88k-dg-dgux5.4R3.10.  */
#undef BSD_PGRPS

/* arch-tag: c11938c9-0cb0-4652-88aa-7eb80bf1cda9
   (do not change this comment) */

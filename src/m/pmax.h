/* Machine description file for DEC MIPS machines.

   Copyright (C) 1992, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
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


#include "mips.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
The operating system would be either osf1, ultrix, or NetBSD.
NOTE-END  */

#ifndef __MIPSEB__
#undef WORDS_BIG_ENDIAN
#endif
#if defined (__NetBSD__)
#define BROKEN_NOCOMBRELOC
#else
#undef LIB_STANDARD
#undef START_FILES
#endif
#undef COFF
#undef TERMINFO
#define MAIL_USE_FLOCK
#define HAVE_UNION_WAIT


#ifdef MACH
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#else
#if !defined (__NetBSD__)
/* This line starts being needed with ultrix 4.0.  */
/* You must delete it for version 3.1.  */
#define START_FILES pre-crt0.o /usr/lib/cmplrs/cc/crt0.o
#endif
#endif

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

#if 0
/* I don't see any such conflict in Ultrix 4.2, 4.2a, or 4.3.  And
   the relocating allocator is a real win.  -JimB  */

/* In Ultrix 4.1, XvmsAlloc.o in libX11.a seems to insist
   on defining malloc itself.  This should avoid conflicting with it.  */
#define SYSTEM_MALLOC
#endif

/* Override what mips.h says about this.  */
#if !defined (__NetBSD__)
#undef LINKER
#endif

#ifdef ultrix
/* Ultrix 4.2 (perhaps also 4.1) implements O_NONBLOCK
   but it doesn't work right;
   and it causes hanging in read_process_output.  */
#define BROKEN_O_NONBLOCK
#endif

#ifndef __NetBSD__
/* mcc@timessqr.gc.cuny.edu says this makes Emacs work with DECnet.  */
#ifdef HAVE_LIBDNET
#define LIBS_MACHINE -ldnet
#endif

/* mcc@timessqr.gc.cuny.edu says it is /vmunix on Ultrix 4.2a.  */
#undef KERNEL_FILE
#define KERNEL_FILE "/vmunix"
#endif

#ifdef ultrix
/* Jim Wilson writes:
   [...] The X11 include files that Dec distributes with Ultrix
   are bogus.

   When __STDC__ is defined (which is true with gcc), the X11 include files
   try to define prototypes.  The prototypes however use types which haven't
   been defined yet, and thus we get syntax/parse errors.

   You can not fix this by changing the include files, because the prototypes
   create circular dependencies, in particular Xutil.h depends on types defined
   in Xlib.h, and Xlib.h depends on types defined in Xutil.h.  So, no matter
   which order you try to include them in, it will still fail.

   Compiling with -DNeedFunctionPrototypes=0 will solve the problem by
   directly inhibiting the bad prototypes.  This could perhaps just be put in
   an a Ultrix configuration file.

   Using the MIT X11 distribution instead of the one provided by Dec will
   also solve the problem, but I doubt you can convince everyone to do this. */
/* Addendum: the MIT X11 distribution neglects to define certain symbols
   when NeedFunctionPrototypes is 0, but still tries to use them when
   NeedVarargsPrototypes is 1 (which is its default value).  So if we're
   going to disable non-variadic prototypes, we also need to disable
   variadic prototypes.  --kwzh@gnu.ai.mit.edu */
#define C_SWITCH_X_MACHINE -DNeedFunctionPrototypes=0 -DNeedVarargsPrototypes=0
#endif

/* Enable a fix in process.c.  */
#define SET_CHILD_PTY_PGRP

/* arch-tag: 45d5070e-d2b7-479f-b336-3fd497c36e15
   (do not change this comment) */

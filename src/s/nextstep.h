/* Configuration file for the NeXTstep system.
   Copyright (C) 1990, 1995 Free Software Foundation, Inc.

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

#include "bsd4-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  We'll need to undo the bsd one. */

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "next-mach"

#ifndef NeXT
#define NeXT
#endif


/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Defining KERNEL_FILE causes lossage because sys/file.h
   stupidly gets confused by it.  */
#undef KERNEL_FILE

#define SYSTEM_MALLOC

#define environ _environ

/* This should be true for recent NeXT systems.  At least since 3.2.  */
#define HAVE_MACH_MACH_H

#if 0 /* I think these are never used--let's see.  -- rms.  */
/* Mask for address bits within a memory segment */

#define SEGSIZ 0x20000
#define SEGMENT_MASK (SEGSIZ - 1)

#define HAVE_UNIX_DOMAIN

/* Conflicts in process.c between ioctl.h & tty.h use of t_foo fields */

#define NO_T_CHARS_DEFINES

/* This avoids a problem in Xos.h when using co-Xist 3.01.  */
#define X_NOT_POSIX
#endif /* 0 */

/* Definitions for how to link.  */

/* Link this program just by running cc.  */
#define ORDINARY_LINK

#define LD_SWITCH_SYSTEM -X

/* Don't use -lc on the NeXT.  */
#ifdef NS_TARGET /* We use the dynamic libraries under Openstep for Mach 4.0 */
#define LIB_STANDARD
#else
#define LIB_STANDARD -lsys_s
#endif

#define LIB_MATH -lm

#define START_FILES pre-crt0.o

#define LIB_X11_LIB -L/usr/lib/X11 -lX11

/* We don't have a g library either, so override the -lg LIBS_DEBUG switch */

#define LIBS_DEBUG

/* We don't have a libgcc.a, so we can't let LIB_GCC default to -lgcc */

#define LIB_GCC

/* Definitions for how to dump.  */

#define UNEXEC unexnext.o

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START 0
/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext ()
/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata ()

/* Don't include string.h--it causes trouble.  */
#undef HAVE_STRING_H

/* Tell emacs.c not to define abort.  */
#define NO_ABORT

/* arch-tag: 5cd6fed4-a0be-4402-9349-85a80bc01d57
   (do not change this comment) */

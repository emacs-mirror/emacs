/* Configuration file for the NeXT machine.
   Copyright (C) 1990, 2002 Free Software Foundation, Inc.

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


/* Say this machine is a next if not previously defined */

#ifndef NeXT
#define NeXT
#endif

/* Define WORDS_BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

/* Let the compiler tell us.  */
#ifdef __BIG_ENDIAN__
#define WORDS_BIG_ENDIAN
#endif

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* Mask for address bits within a memory segment */

#define SEGSIZ 0x20000
#define SEGMENT_MASK (SEGSIZ - 1)

#define SYSTEM_MALLOC

#define HAVE_UNIX_DOMAIN

#define LIB_X11_LIB -L/usr/lib/X11 -lX11

/* This avoids a problem in Xos.h when using co-Xist 3.01.  */
#define X_NOT_POSIX

/* Conflicts in process.c between ioctl.h & tty.h use of t_foo fields */

#define NO_T_CHARS_DEFINES

/* Use our own unexec routines */

#define UNEXEC unexnext.o

/* We don't have a g library either, so override the -lg LIBS_DEBUG switch */

#define LIBS_DEBUG

/* We don't have a libgcc.a, so we can't let LIB_GCC default to -lgcc */

#define LIB_GCC

#if 0  /* ohl@chico.harvard.edu says to do this.  */
/* Compile "strict bsd" to avoid warnings from include files */

#define C_SWITCH_MACHINE	-bsd
#endif

/* Link this program just by running cc.  */
#define ORDINARY_LINK

/* start_of_text isn't actually used, so make it compile without error.  */
#define TEXT_START 0
/* This seems to be right for end_of_text, but it may not be used anyway.  */
#define TEXT_END get_etext ()
/* This seems to be right for end_of_data, but it may not be used anyway.  */
#define DATA_END get_edata ()

/* Defining KERNEL_FILE causes lossage because sys/file.h
   stupidly gets confused by it.  */
#undef KERNEL_FILE

#define LD_SWITCH_MACHINE -X -noseglinkedit

#define environ _environ

#if 0 /* This is ok for NeXT system version 3.0 or above.  */
/* Where to find the kernel, for load average.  */
#define KERNEL_FILE "/mach"
#endif

/* This should be true for recent NeXT systems.  At least since 3.2.  */
#define HAVE_MACH_MACH_H

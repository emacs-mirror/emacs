/* m- file for pyramid with mips processor
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#define BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */


#define NO_ARG_ARRAY

#undef WORD_MACHINE


/* XINT must explicitly sign extend */

#define EXPLICIT_SIGN_EXTEND

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */


#define SIGN_EXTEND_CHAR(c) ((signed char)(c))


/* pyramid preprocessor defines "pyr", however the following is clearer */
#define pyramid

/* Don't use the union types any more.  They were used until Emacs 17.45.  */

#define NO_UNION_TYPE

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double		/* might be long TODO */

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Don't use the ordinary -g for debugging in cc */
/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

#define CANNOT_DUMP

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

/* these are needed! */
#define HAVE_ALLOCA
#define SYSTEM_MALLOC

/*
 * most of the rest is from m-mips.h
 * but there was too much in that that we didn't want
 */
/* #define NO_REMAP /* don't adjust data_start */


/* The standard definitions of these macros would work ok,
   but these are faster because the constants are short.  */

#define XUINT(a) (((unsigned)(a) << INTBITS-VALBITS) >> INTBITS-VALBITS)

#define XSET(var, type, ptr) \
   ((var) = ((int)(type) << VALBITS) + (((unsigned) (ptr) << INTBITS-VALBITS) >> INTBITS-VALBITS))

#define XSETINT(a, b)  XSET(a, XTYPE(a), b)
#define XSETUINT(a, b) XSET(a, XTYPE(a), b)
#define XSETPNTR(a, b) XSET(a, XTYPE(a), b)

#define XUNMARK(a) ((a) = (((unsigned)(a) << INTBITS-GCTYPEBITS-VALBITS) >> INTBITS-GCTYPEBITS-VALBITS))

/* Cancel certain parts of standard sysV support.  */
#undef NONSYSTEM_DIR_LIBRARY
#define SYSV_SYSTEM_DIR
#undef static

/* Don't try to use SIGIO or FIONREAD even though they are defined.  */
#undef SIGIO
#define BROKEN_FIONREAD

/* Describe special kernel features.  */

#define HAVE_SYSVIPC

#define C_DEBUG_SWITCH -gx


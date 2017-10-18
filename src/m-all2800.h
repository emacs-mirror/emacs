/* m-alliant-2800.h  Alliant FX/2800 machine running Concentrix 2800.
   Copyright (C) 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* The following three symbols give information on
 the size of various data types.  */

#define SHORTBITS 16		/* Number of bits in a short */

#define INTBITS 32		/* Number of bits in an int */

#define LONGBITS 32		/* Number of bits in a long */

/* Define BIG_ENDIAN iff lowest-numbered byte in a word
   is the most significant byte.  */

#undef BIG_ENDIAN

/* Define NO_ARG_ARRAY if you cannot take the address of the first of a
 * group of arguments and treat it as an array of the arguments.  */

#define NO_ARG_ARRAY

/* Define WORD_MACHINE if addresses and such have
 * to be corrected before they can be used as byte counts.  */

#undef WORD_MACHINE

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) (c)

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

#define ALLIANT
#define ALLIANT_2800
#define sun			/* Use X support for Sun keyboard stuff. */
#define C_OPTIMIZE_SWITCH -Og	/* No concurrent code allowed here. */
#define VALBITS 26
#define GCTYPEBITS 5

/* Use type int rather than a union, to represent Lisp_Object */
/* This is desirable for most machines.  */

#define NO_UNION_TYPE

/* Define EXPLICIT_SIGN_EXTEND if XINT must explicitly sign-extend
   the 24-bit bit field into an int.  In other words, if bit fields
   are always unsigned.

   If you use NO_UNION_TYPE, this flag does not matter.  */
/* On Alliants, bitfields are unsigned. */

#define EXPLICIT_SIGN_EXTEND

/* Concentrix uses a different kernel symbol for load average. */

#undef  LDAV_SYMBOL		/* Undo definition in s-bsd4-2.h */
#define LDAV_SYMBOL "_Loadavg"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (x * 100 / LOADAVG_SCALE)

/* include <sys/param.h> for the definition of LOADAVG_SCALE, and also
   LOADAVG_SIZE, the number of items in the Loadavg array. */

/* Define CANNOT_DUMP on machines where unexec does not work.
   Then the function dump-emacs will not be defined
   and temacs will do (load "loadup") automatically unless told otherwise.  */

/* #define CANNOT_DUMP */
#define UNEXEC unexfx2800.o
#define LIBS_MACHINE -lalliant

/* Define VIRT_ADDR_VARIES if the virtual addresses of
   pure and impure space as loaded can vary, and even their
   relative order cannot be relied on.

   Otherwise Emacs assumes that text space precedes data space,
   numerically.  */

#undef VIRT_ADDR_VARIES

/* Define C_ALLOCA if this machine does not support a true alloca
   and the one written in C should be used instead.
   Define HAVE_ALLOCA to say that the system provides a properly
   working alloca function and it should be used.
   Define neither one if an assembler-language alloca
   in the file alloca.s should be used.  */

#undef C_ALLOCA
#define HAVE_ALLOCA

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
/* Actually, Alliant CONCENTRIX does paging "right":
   data pages are copy-on-write, which means that the pure data areas
   are shared automatically and remapping is not necessary.  */

#define NO_REMAP

/* Alliant needs special crt0.o because system version is not reentrant */

#define START_FILES crt0.o

/* Alliant dependent code for dumping executing image.
   See crt0.c code for alliant.  */

#define ADJUST_EXEC_HEADER {\
extern int _curbrk, _setbrk;\
_setbrk = _curbrk;\
hdr.a_bss_addr = bss_start;\
unexec_text_start = hdr.a_text_addr;}

/* POSIX Compatibility */
/* Use System V.4 style getdents/readdir <dirent.h> for 2.2 and up. */
#define SYSV_SYSTEM_DIR
#define HAVE_CLOSEDIR

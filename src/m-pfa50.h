/* m- file for PFU A-series.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* 68000 has lowest-numbered byte as most significant */

#define BIG_ENDIAN

/* Define how to take a char and sign-extend into an int.
   On machines where char is signed, this is a no-op.  */

#define SIGN_EXTEND_CHAR(c) ((((int) (c)) << 24) >> 24)

/* Say this machine is a 68000 */

#define m68000
#define mc68000 1

/* Use type int rather than a union, to represent Lisp_Object */

#define NO_UNION_TYPE

/* XINT must explicitly sign-extend */

#define EXPLICIT_SIGN_EXTEND

/* The symbol FIONREAD is defined, but the feature does not work.  */

#define BROKEN_FIONREAD

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Define TEXT_START_ADDR if your linker don't set execute point to _start.
   If it needed, temacs always CORE-DUMP.	*/

#define TEXT_START_ADDR __start

/* Define START_FILES if your machine used _start.
 */

#define START_FILES crt0.o

/* Define LD_SWITCH_MACHINE if your linker need it.
 */

#define LD_SWITCH_MACHINE -e __start

#if	pfa50 || pfa70

/* On A-50/60/70/80, data space has high order byte use. */
#define VALMASK (((1<<VALBITS) - 1) | 0x60000000)
#define XTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & 0x1f))
#define XGCTYPE(a) ((enum Lisp_Type) (((a) >> VALBITS) & 0x1f))

#endif /* pfa50, pfa70 */

/* SX/A has alloca in the PW library.  */

#define LIB_STANDARD -lPW -lc
#define HAVE_ALLOCA

/* SX/A uses terminfo and lib/curses   */

#define TERMINFO

#define HAVE_TIMEVAL
#define HAVE_SELECT
#define HAVE_PTYS
#define HAVE_SOCKETS
#define HAVE_RENAME

/* SX/A use SystemV style getdents/readdir. */
#define HAVE_CLOSEDIR

/* SX/A does not have sigpause.  */
#undef HAVE_SYSV_SIGPAUSE

/* SX/A does not have sigblock.  */
#define sigblock(mask)	(0)

#define USG_SYS_TIME
#define USE_UTIME

#define NO_SIOCTL_H

#undef SIGIO

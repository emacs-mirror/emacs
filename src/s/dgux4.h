/* Definitions file for GNU Emacs running on Data General's DG/UX
   Release 4.10 and above.
   Copyright (C) 1996 Free Software Foundation, Inc.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* This file was written by Roderick Schertler <roderick@ibcinc.com>,
   contact me if you have problems with or comments about running Emacs
   on dgux.

   A number of things in the older dgux*.h files don't make sense to me,
   but since I'm relying on memory and I don't have any older dgux
   systems installed on which to test changes I'm undoing or fixing them
   here rather than fixing them at the source. */

/* In dgux.h it says "Can't use sys_signal because then etc/server.c
   would need sysdep.o." and then it #defines signal() to be
   berk_signal(), but emacsserver.c does `#undef signal' anyway, so that
   doesn't make sense.

   Further, sys_signal() in sysdep.c already had a special case for
   #ifdef DGUX, it called berk_signal() explicitly.  I've removed that
   special case because it also didn't make sense:  All versions of dgux
   which the dgux*.h headers take into account have POSIX signals
   (POSIX_SIGNALS is #defined in dgux.h).  The comments in sys_signal()
   even acknowledged this (saying that the special berk_signal() case
   wasn't really necessary), they said that sys_signal() was using
   berk_signal() instead of sigaction() for efficiency.  Since both give
   reliable signals neither has to be invoked within the handler.  If
   the efficiency that the comments were talking about is the overhead
   of setting up the sigaction struct rather than just passing the
   function pointer in (which is the only efficiency I can think of)
   then that's a needless optimization, the Emacs sources do better
   without the special case.

   The following definition will prevent dgux.h from re-defining
   signal().  I can't just say `#undef signal' after including dgux.h
   because signal() is already a macro, defined in <sys/signal.h>, and
   the original definition would be lost. */
#define NO_DGUX_SIGNAL_REDEF

#include "dgux5-4-3.h"

#define LIBS_DEBUG /* nothing, -lg doesn't exist */
#define LIBS_SYSTEM -lsocket -lnsl

#ifndef NOT_C_CODE

/* dgux.h defines _setjmp() to be sigsetjmp(), but it defines _longjmp
   to be longjmp() rather than siglongjmp().  Further, it doesn't define
   jmp_buf, so sigsetjmp() is being called with a jmp_buf rather than a
   sigjmp_buf, and the buffer is then passed to vanilla longjmp().  This
   provides a more complete emulation of the Berkeley semantics. */

#include <setjmp.h>
#undef jmp_buf
#undef _setjmp
#undef  setjmp
#undef _longjmp
#undef  longjmp
#define jmp_buf		sigjmp_buf
#define _setjmp(env)	sigsetjmp(env, 0)
#define  setjmp(env)	sigsetjmp(env, 1)
#define _longjmp	siglongjmp
#define  longjmp	siglongjmp

/* The BAUD_CONVERT definition in dgux.h is wrong with this version
   of dgux, but I'm not sure when it changed.

   With the current system Emacs' standard handling of ospeed and
   baud_rate don't work.  The baud values (B9600 and so on) returned by
   cfgetospeed() aren't compatible with those used by ospeed.  speed_t,
   the type returned by cfgetospeed(), is unsigned long and speed_t
   values are large.  Further, it isn't possible to get at both the
   SysV3 (ospeed) and POSIX (cfgetospeed()) values through symbolic
   constants simultaneously because they both use the same names
   (B9600).  To get both baud_rate and ospeed right at the same time
   it's necessary to hardcode the values for one set of values, here I'm
   hardcoding ospeed. */
#undef BAUD_CONVERT
#define INIT_BAUD_RATE()					\
    struct termios sg;						\
								\
    tcgetattr (input_fd, &sg);					\
    switch (cfgetospeed (&sg)) {				\
    case    B50:	baud_rate =    50; ospeed = 0x1; break; \
    case    B75:	baud_rate =    75; ospeed = 0x2; break; \
    case   B110:	baud_rate =   110; ospeed = 0x3; break; \
    case   B134:	baud_rate =   134; ospeed = 0x4; break; \
    case   B150:	baud_rate =   150; ospeed = 0x5; break; \
    case   B200:	baud_rate =   200; ospeed = 0x6; break; \
    case   B300:	baud_rate =   300; ospeed = 0x7; break; \
    case   B600:	baud_rate =   600; ospeed = 0x8; break; \
    default:							\
    case  B1200:	baud_rate =  1200; ospeed = 0x9; break; \
    case  B1800:	baud_rate =  1800; ospeed = 0xa; break; \
    case  B2400:	baud_rate =  2400; ospeed = 0xb; break; \
    case  B4800:	baud_rate =  4800; ospeed = 0xc; break; \
    case  B9600:	baud_rate =  9600; ospeed = 0xd; break; \
    case B19200:	baud_rate = 19200; ospeed = 0xe; break; \
    case B38400:	baud_rate = 38400; ospeed = 0xf; break; \
    }								\
    return;


#if 0 /* Ehud Karni <ehud@unix.simonwiesel.co.il> says that the problem
	 still exists on m88k-dg-dguxR4.11MU04 and i586-dg-dguxR4.11MU04.  */
/* The `stop on tty output' problem which occurs when using
   INTERRUPT_INPUT and when Emacs is invoked under X11 using a job
   control shell (csh, ksh, etc.) in the background doesn't look to be
   present in R4.11.  (At least, I can't reproduce it using jsh, csh,
   ksh or zsh.) */
#undef BROKEN_FIONREAD
#define INTERRUPT_INPUT
#endif /* 0 - never */

/* In R4.11 (or maybe R4.10, I don't have a system with that version
   loaded) some of the internal stdio semantics were changed.  One I
   found while working on MH is that _cnt has to be 0 before _filbuf()
   is called.  Another is that (_ptr - _base) doesn't indicate how many
   characters are waiting to be sent.  I can't spot a good way to get
   that info from the FILE internals. */
#define PENDING_OUTPUT_COUNT(FILE) (1)

#endif /* NOT_C_CODE */

/* arch-tag: c7013e7b-6e2e-44f2-ba61-90b6d5e2ea45
   (do not change this comment) */

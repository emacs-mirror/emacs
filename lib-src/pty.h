/* Terminal initialization for emacsclient.
   Copyright (C) 2003 Free Software Foundation, Inc.

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

/* Adapted from systty.h */


#ifdef HAVE_TERMIOS
#define HAVE_TCATTR
#endif


/* Include the proper files.  */
#ifdef HAVE_TERMIO
#ifdef __DGUX
#include <sys/ioctl.h>
#endif
#ifndef NO_TERMIO
#include <termio.h>
#endif /* not NO_TERMIO */
#ifndef INCLUDED_FCNTL
#define INCLUDED_FCNTL
#include <fcntl.h>
#endif
#else /* not HAVE_TERMIO */
#ifdef HAVE_TERMIOS
#if defined(_AIX) && defined(_I386)
#include <termios.h>		/* termios.h needs to be before termio.h */
#include <termio.h>
#else /* not (_AIX && _I386) */
#ifndef NO_TERMIO
#include <termio.h>
#endif
#include <termios.h>
#endif /* not (_AIX && _I386) */
#define INCLUDED_FCNTL
#include <fcntl.h>
#else /* neither HAVE_TERMIO nor HAVE_TERMIOS */
#ifndef VMS
#ifndef DOS_NT
#include <sgtty.h>
#endif /* not DOS_NT */
#else /* VMS */
#include <descrip.h>
static struct iosb
{
  short status;
  short offset;
  short termlen;
  short term;
} input_iosb;

extern int waiting_for_ast;
extern int stop_input;
extern int input_ef;
extern int timer_ef;
extern int process_ef;
extern int input_eflist;
extern int timer_eflist;

static $DESCRIPTOR (input_dsc, "TT");
static int terminator_mask[2] = { 0, 0 };

static struct sensemode {
  short status;
  unsigned char xmit_baud;
  unsigned char rcv_baud;
  unsigned char crfill;
  unsigned char lffill;
  unsigned char parity;
  unsigned char unused;
  char class;
  char type;
  short scr_wid;
  unsigned long tt_char : 24, scr_len : 8;
  unsigned long tt2_char;
} sensemode_iosb;
#endif /* VMS */
#endif /* not HAVE_TERMIOS */
#endif /* not HAVE_TERMIO */

#ifdef __GNU_LIBRARY__
#include <sys/ioctl.h>
#include <termios.h>
#endif

#ifdef AIXHFT
/* Get files for keyboard remapping */
#define HFNKEYS 2
#include <sys/hft.h>
#include <sys/devinfo.h>
#endif

/* Get rid of LLITOUT in 4.1, since it is said to stimulate kernel bugs.  */
#ifdef BSD4_1
#undef LLITOUT
#define LLITOUT 0
#endif /* 4.1 */

#ifdef NEED_BSDTTY
#include <sys/bsdtty.h>
#endif

#if defined (HPUX) && defined (HAVE_PTYS)
#include <sys/ptyio.h>
#endif

#ifdef AIX
#include <sys/pty.h>
#endif /* AIX */

#if (defined (POSIX) || defined (NEED_UNISTD_H)) && defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif

#ifdef SYSV_PTYS
#include <sys/types.h>
#include <sys/tty.h>
#ifdef titan
#include <sys/ttyhw.h>
#include <sys/stream.h>
#endif
#ifndef NO_PTY_H
#include <sys/pty.h>
#endif
#endif

/* saka@pfu.fujitsu.co.JP writes:
   FASYNC defined in this file. But, FASYNC don't working.
   so no problem, because unrequest_sigio only need. */
#if defined (pfa)
#include <sys/file.h>
#endif


/* Special cases - inhibiting the use of certain features.  */

#ifdef APOLLO
#undef TIOCSTART
#endif

#ifdef XENIX
#undef TIOCGETC  /* Avoid confusing some conditionals that test this.  */
#endif

#ifdef BROKEN_TIOCGETC
#undef TIOCGETC  /* Avoid confusing some conditionals that test this.  */
#endif

/* UNIPLUS systems may have FIONREAD.  */
#ifdef UNIPLUS
#include <sys.ioctl.h>
#endif

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#undef ASYNC
#endif

/* Interrupt input is not used if there is no FIONREAD.  */
#ifndef FIONREAD
#undef SIGIO
#endif

/* On TERMIOS systems, the tcmumbleattr calls take care of these
   parameters, and it's a bad idea to use them (on AIX, it makes the
   tty hang for a long time).  */
#if defined (TIOCGLTC) && !defined (HAVE_TERMIOS)
#define HAVE_LTCHARS
#endif

#if defined (TIOCGETC) && !defined (HAVE_TERMIOS)
#define HAVE_TCHARS
#endif


/* Try to establish the correct character to disable terminal functions
   in a system-independent manner.  Note that USG (at least) define
   _POSIX_VDISABLE as 0!  */

#ifdef _POSIX_VDISABLE
#define CDISABLE _POSIX_VDISABLE
#else /* not _POSIX_VDISABLE */
#ifdef CDEL
#undef CDISABLE
#define CDISABLE CDEL
#else /* not CDEL */
#define CDISABLE 255
#endif /* not CDEL */
#endif /* not _POSIX_VDISABLE */

/* Get the number of characters queued for output.  */

/* EMACS_OUTQSIZE(FD, int *SIZE) stores the number of characters
   queued for output to the terminal FD in *SIZE, if FD is a tty.
   Returns -1 if there was an error (i.e. FD is not a tty), 0
   otherwise.  */
#ifdef TIOCOUTQ
#define EMACS_OUTQSIZE(fd, size) (ioctl ((fd), TIOCOUTQ, (size)))
#endif

#ifdef HAVE_TERMIO
#ifdef TCOUTQ
#undef EMACS_OUTQSIZE
#define EMACS_OUTQSIZE(fd, size) (ioctl ((fd), TCOUTQ, (size)))
#endif
#endif

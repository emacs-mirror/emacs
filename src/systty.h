/* systty.h - System-dependent definitions for terminals.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef HAVE_TERMIOS
#define HAVE_TCATTR
#endif

/* If we defined these before and we are about to redefine them,
   prevent alarming warnings.  */
#ifdef BSD_TERMIOS
#undef NL0
#undef NL1
#undef CR0
#undef CR1
#undef CR2
#undef CR3
#undef TAB0
#undef TAB1
#undef TAB2
#undef XTABS
#undef BS0
#undef BS1
#undef FF0
#undef FF1
#undef ECHO
#undef NOFLSH
#undef TOSTOP
#undef FLUSHO
#undef PENDIN
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
#else /* not HAVE_TERMIOS */
#ifndef NO_TERMIO
#include <termio.h>
#endif
#include <termios.h>
#endif /* _AIX && _I386 */
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
#include <termios.h>
#endif

#ifdef AIX
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


/* Manipulate a terminal's current process group.  */

/* EMACS_HAVE_TTY_PGRP is true if we can get and set the tty's current
   controlling process group.

   EMACS_GET_TTY_PGRP(int FD, int *PGID) sets *PGID the terminal FD's
   current process group.  Return -1 if there is an error.

   EMACS_SET_TTY_PGRP(int FD, int *PGID) sets the terminal FD's
   current process group to *PGID.  Return -1 if there is an error.  */

#ifdef HPUX
/* HPUX tty process group stuff doesn't work, says the anonymous voice
   from the past.  */
#else
#ifdef TIOCGPGRP
#define EMACS_HAVE_TTY_PGRP
#else
#ifdef HAVE_TERMIOS
#define EMACS_HAVE_TTY_PGRP
#endif
#endif
#endif

#ifdef EMACS_HAVE_TTY_PGRP

#if defined (HAVE_TERMIOS) && ! defined (BSD_TERMIOS)

#define EMACS_GET_TTY_PGRP(fd, pgid) (*(pgid) = tcgetpgrp ((fd)))
#define EMACS_SET_TTY_PGRP(fd, pgid) (tcsetpgrp ((fd), *(pgid)))

#else
#ifdef TIOCSPGRP

#define EMACS_GET_TTY_PGRP(fd, pgid) (ioctl ((fd), TIOCGPGRP, (pgid)))
#define EMACS_SET_TTY_PGRP(fd, pgid) (ioctl ((fd), TIOCSPGRP, (pgid)))

#endif
#endif

#else

/* Just ignore this for now and hope for the best */
#define EMACS_GET_TTY_PGRP(fd, pgid) 0
#define EMACS_SET_TTY_PGRP(fd, pgif) 0

#endif

/* EMACS_GETPGRP (arg) returns the process group of the terminal.  */

#if defined (USG) && !defined (GETPGRP_NEEDS_ARG)
#  if !defined (GETPGRP_NO_ARG)
#    define GETPGRP_NO_ARG
#  endif
#endif

#if defined (GETPGRP_NO_ARG)
#  define EMACS_GETPGRP(x) getpgrp()
#else
#  define EMACS_GETPGRP(x) getpgrp(x)
#endif /* !GETPGRP_NO_ARG */

/* Manipulate a TTY's input/output processing parameters.  */

/* struct emacs_tty is a structure used to hold the current tty
   parameters.  If the terminal has several structures describing its
   state, for example a struct tchars, a struct sgttyb, a struct
   tchars, a struct ltchars, and a struct pagechars, struct
   emacs_tty should contain an element for each parameter struct
   that Emacs may change.

   EMACS_GET_TTY (int FD, struct emacs_tty *P) stores the parameters
   of the tty on FD in *P.  Return zero if all's well, or -1 if we ran
   into an error we couldn't deal with.

   EMACS_SET_TTY (int FD, struct emacs_tty *P, int waitp)
   sets the parameters of the tty on FD according to the contents of
   *P.  If waitp is non-zero, we wait for all queued output to be
   written before making the change; otherwise, we forget any queued
   input and make the change immediately.
   Return 0 if all went well, and -1 if anything failed.

   EMACS_TTY_TABS_OK (struct emacs_tty *P) is false iff the kernel
   expands tabs to spaces upon output; in that case, there is no
   advantage to using tabs over spaces.  */


/* For each tty parameter structure that Emacs might want to save and restore,
   - include an element for it in this structure, and
   - extend the emacs_{get,set}_tty functions in sysdep.c to deal with the
     new members.  */

struct emacs_tty {

/* There is always one of the following elements, so there is no need
   for dummy get and set definitions.  */
#ifdef HAVE_TCATTR
  struct termios main;
#else
#ifdef HAVE_TERMIO
  struct termio main;
#else
#ifdef VMS
  struct sensemode main;
#else
#ifdef DOS_NT
  int main;
#else  /* not DOS_NT */
  struct sgttyb main;
#endif /* not DOS_NT */
#endif
#endif
#endif

/* If we have TERMIOS, we don't need to do this - they're taken care of
   by the tc*attr calls.  */
#ifndef HAVE_TERMIOS
#ifdef HAVE_LTCHARS
  struct ltchars ltchars;
#endif

#ifdef HAVE_TCHARS
  struct tchars tchars;
  int lmode;
#endif
#endif
};

/* Define EMACS_GET_TTY and EMACS_SET_TTY,
   the macros for reading and setting parts of `struct emacs_tty'.

   These got pretty unmanageable (huge macros are hard to debug), and
   finally needed some code which couldn't be done as part of an
   expression, so we moved them out to their own functions in sysdep.c.  */
#define EMACS_GET_TTY(fd, p)        (emacs_get_tty ((fd), (p)))
#define EMACS_SET_TTY(fd, p, waitp) (emacs_set_tty ((fd), (p), (waitp)))


/* Define EMACS_TTY_TABS_OK.  */

#ifdef HAVE_TERMIOS

#ifdef TABDLY
#define EMACS_TTY_TABS_OK(p) (((p)->main.c_oflag & TABDLY) != TAB3)
#else
#define EMACS_TTY_TABS_OK(p) 1
#endif

#else /* not def HAVE_TERMIOS */
#ifdef HAVE_TERMIO

#define EMACS_TTY_TABS_OK(p) (((p)->main.c_oflag & TABDLY) != TAB3)

#else /* neither HAVE_TERMIO nor HAVE_TERMIOS */
#ifdef VMS

#define EMACS_TTY_TABS_OK(p) (((p)->main.tt_char & TT$M_MECHTAB) != 0)

#else

#ifdef DOS_NT
#define EMACS_TTY_TABS_OK(p) 0
#else /* not DOS_NT */
#define EMACS_TTY_TABS_OK(p) (((p)->main.sg_flags & XTABS) != XTABS)
#endif /* not DOS_NT */

#endif /* not def VMS */
#endif /* not def HAVE_TERMIO */
#endif /* not def HAVE_TERMIOS */

/* systty.h - System-dependent definitions for terminals.
   Copyright (C) 1992 Free Software Foundation, Inc.

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


/* Include the proper files.  */
#ifdef HAVE_TERMIO
#include <termio.h>
#include <fcntl.h>
#else
#ifdef HAVE_TERMIOS
#include <termio.h>
#include <termios.h>
#else /* neither HAVE_TERMIO nor HAVE_TERMIOS */
#ifndef VMS
#include <sgtty.h>
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
extern int input_ef = 0;
extern int timer_ef = 0;
extern int process_ef = 0;
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
#include <unistd.h>
#endif /* AIX */

#ifdef SYSV_PTYS
#include <sys/tty.h>
#ifdef titan
#include <sys/ttyhw.h>
#include <sys/stream.h>
#endif
#include <sys/pty.h>
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

/* Interupt input is not used if there is no FIONREAD.  */
#ifndef FIONREAD
#undef SIGIO
#endif


/* Try to establish the correct character to disable terminal functions
   in a system-independent manner.  Note that USG (at least) define
   _POSIX_VDISABLE as 0!  */

#ifdef _POSIX_VDISABLE
#define CDISABLE _POSIX_VDISABLE
#else /* not _POSIX_VDISABLE */
#ifdef CDEL
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

#ifdef HAVE_TERMIOS

#define EMACS_GET_TTY_PGRP(fd, pgid) (*(pgid) = tcgetpgrp ((fd)))
#define EMACS_SET_TTY_PGRP(fd, pgid) (*(pgid) = tcsetpgrp ((fd)))

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


/* Manipulate a TTY's input/output processing parameters.  */

/* struct emacs_tty is a structure used to hold the current tty
   parameters.  If the terminal has several structures describing its
   state, for example a struct tchars, a struct sgttyb, a struct
   tchars, a struct ltchars, and a struct pagechars, struct
   emacs_tty should contain an element for each parameter struct
   that Emacs may change.

   EMACS_GET_TTY (int FD, struct emacs_tty *P) stores the
   parameters of the tty on FD in *P.

   EMACS_SET_TTY (int FD, struct emacs_tty *P, int waitp)
   sets the parameters of the tty on FD according to the contents of
   *P.  If waitp is non-zero, we wait for all queued output to be
   written before making the change; otherwise, we forget any queued
   input and make the change immediately.

   EMACS_TTY_TABS_OK (struct emacs_tty *P) is false iff the kernel
   expands tabs to spaces upon output; in that case, there is no
   advantage to using tabs over spaces.  */



/* For each tty parameter structure that Emacs might want to save and restore,
   - include an element for it in this structure,
   - define a pair of numbered macros to get and set it and return 
     true iff the call succeeded,
   - give alternative definitions for when the component is not implemented
     which always succeed, and
   - extend the definition of EMACS_{GET,SET}_TTY_CHARS to include the
     new macros.  */

struct emacs_tty {

/* There is always one of the following elements, so there is no need
   for dummy get and set definitions.  */
#ifdef HAVE_TERMIOS
  struct termios main;
#else
#ifdef HAVE_TERMIO
  struct termio main;
#else
#ifdef VMS
  struct sensemode main;
#else
  struct sgttyb main;
#endif
#endif
#endif

#ifdef HAVE_TERMIOS
#define HAVE_TCATTR
#endif

#ifdef HAVE_TCATTR

#define EMACS_GET_TTY_1(fd, p) (tcgetattr ((fd), &(p)->main) != -1)
#define EMACS_SET_TTY_1(fd, p, waitp) \
  (tcsetattr ((fd), (waitp) ? TCSAFLUSH : TCSADRAIN, &(p)->main) != -1)

#else
#ifdef HAVE_TERMIO

#define EMACS_GET_TTY_1(fd, p) (ioctl ((fd), TCGETA, &(p)->main) != -1)
#define EMACS_SET_TTY_1(fd, p, waitp)			\
  (ioctl ((fd), (waitp) ? TCSETAW : TCSETAF, &(p)->main) != -1)

#else
#ifdef VMS

/* These definitions will really only work in sysdep.c, because of their
   use of input_iosb.  I don't know enough about VMS QIO to fix this.  */
#define EMACS_GET_TTY_1(fd, p)					\
  (1 & SYS$QIOW (0, (fd), IO$_SENSEMODE, (p), 0, 0,		\
	    &(p)->main.class, 12, 0, 0, 0, 0))
#define EMACS_SET_TTY_1(fd, p, waitp)				\
  (1 & SYS$QIOW (0, (fd), IO$_SETMODE, &input_iosb, 0, 0,	\
	    &(p)->main.class, 12, 0, 0, 0, 0))

#else

#define EMACS_GET_TTY_1(fd, p) (ioctl ((fd), TIOCGETP, &(p)->main) != -1)
#define EMACS_SET_TTY_1(fd, p, waitp)			\
  (ioctl ((fd), (waitp) ? TIOCSETP : TIOCSETN, &(p)->main) != -1)

#endif
#endif
#endif

#ifdef TIOCGLTC
  struct ltchars ltchars;
#define EMACS_GET_TTY_2(fd, p)				\
  (ioctl ((fd), TIOCGLTC, &(p)->ltchars) != -1)
#define EMACS_SET_TTY_2(fd, p, waitp)			\
  (ioctl ((fd), TIOCSLTC, &(p)->ltchars) != -1)
#else
#define EMACS_GET_TTY_2(fd, p) 1
#define EMACS_SET_TTY_2(fd, p, waitp) 1
#endif /* TIOCGLTC */

#ifdef TIOCGETC
  struct tchars tchars;
  int lmode;
#define EMACS_GET_TTY_3(fd, p)				\
  (ioctl ((fd), TIOCGETC, &(p)->tchars) != -1		\
   && ioctl ((fd), TIOCLGET, &(p)->lmode) != -1)
#define EMACS_SET_TTY_3(fd, p, waitp)			\
  (ioctl ((fd), TIOCSETC, &(p)->tchars) != -1		\
   && ioctl ((fd), TIOCLSET, &(p)->lmode) != -1)
#else
#define EMACS_GET_TTY_3(fd, p) 1
#define EMACS_SET_TTY_3(fd, p, waitp) 1
#endif /* TIOCGLTC */

};

/* Define these to be a concatenation of all the EMACS_{GET,SET}_TTY_n
   macros.  */
#define EMACS_GET_TTY(fd, tc)	\
  (EMACS_GET_TTY_1 (fd, tc)	\
   && EMACS_GET_TTY_2 (fd, tc)	\
   && EMACS_GET_TTY_3 (fd, tc))

#define EMACS_SET_TTY(fd, tc, waitp)	\
  (EMACS_SET_TTY_1 (fd, tc, waitp)	\
   && EMACS_SET_TTY_2 (fd, tc, waitp)	\
   && EMACS_SET_TTY_3 (fd, tc, waitp))


#ifdef HAVE_TERMIOS

#define EMACS_TTY_TABS_OK(p) (((p)->main.c_oflag & TABDLY) != TAB3)

#else /* not def HAVE_TERMIOS */
#ifdef HAVE_TERMIO

#define EMACS_TTY_TABS_OK(p) (((p)->main.c_oflag & TABDLY) != TAB3)

#else /* neither HAVE_TERMIO nor HAVE_TERMIOS */
#ifdef VMS

#define EMACS_TTY_TABS_OK(p) (((p)->main.tt_char & TT$M_MECHTAB) != 0)

#else

#define EMACS_TTY_TABS_OK(p) (((p)->main.sg_flags & XTABS) != XTABS)

#endif /* not def VMS */
#endif /* not def HAVE_TERMIO */
#endif /* not def HAVE_TERMIOS */

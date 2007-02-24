/* Definitions file for GNU Emacs running on osf1.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006,
                 2007  Free Software Foundation, Inc.

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

/* Identify OSF1 for the m- files. */

#define OSF1

#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd

#define GETPGRP_NO_ARG

#define SYSV_SYSTEM_DIR

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  We convert it to a -rpath option
   which is what OSF1 uses.  */
#define LD_SWITCH_SYSTEM `echo LD_SWITCH_X_SITE_AUX | sed -e 's/-R/-Wl,-rpath,/'`

#define HAVE_TERMIOS

#ifndef __GNUC__
/* Optimize, inaccurate debugging.  */
#define C_DEBUG_SWITCH -g3
#endif

#ifndef NOT_C_CODE
#ifndef OSF5			/* fixed in 5.0 */
/* Hack alert!  For reasons unknown to mankind the string.h file insists
   on defining bcopy etc. as taking char pointers as arguments.  With
   Emacs this produces an endless amount of warning which are harmless,
   but tends to flood the real errors.  This hack works around this problem
   by not prototyping.  */
#define bcopy string_h_bcopy
#define bzero string_h_bzero
#define bcmp  string_h_bcmp
#include <string.h>
#undef bcopy
#undef bzero
#undef bcmp
#endif
#endif

#define ORDINARY_LINK

/* Some systems seem to have this, others don't.  */
#ifdef HAVE_LIBDNET
#define LIBS_MACHINE -ldnet
#else
#define LIBS_MACHINE -ldnet_stub
#endif

#define LIBS_DEBUG
#define START_FILES pre-crt0.o

#define PTY_ITERATION		for (i = 0; i < 1; i++) /* ick */
#define PTY_NAME_SPRINTF	/* none */
#define PTY_TTY_NAME_SPRINTF	/* none */
#define PTY_OPEN					\
  do							\
    {							\
      int dummy;					\
      SIGMASKTYPE mask;					\
      mask = sigblock (sigmask (SIGCHLD));		\
      if (-1 == openpty (&fd, &dummy, pty_name, 0, 0))	\
	fd = -1;					\
      sigsetmask (mask);				\
      emacs_close (dummy);				\
    }							\
  while (0)

/* arch-tag: 65eaea67-fcc3-4de7-8574-d46beb82d4ed
   (do not change this comment) */

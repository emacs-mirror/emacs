/* systty.h - System-dependent definitions for terminals.
   Copyright (C) 1993-1994, 2001-2018 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_SYSTTY_H
#define EMACS_SYSTTY_H

/* Include the proper files.  */

#ifndef DOS_NT
#include <termios.h>
#include <fcntl.h>
#endif /* not DOS_NT */

#include <sys/ioctl.h>

#ifdef HPUX
#include <sys/bsdtty.h>
#include <sys/ptyio.h>
#endif

#ifdef AIX
#include <sys/pty.h>
#endif /* AIX */

#include <unistd.h>

#include "lisp.h"


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

/* Manipulate a TTY's input/output processing parameters.  */

/* struct emacs_tty is a structure used to hold the current tty
   parameters.  If the terminal has several structures describing its
   state, for example a struct tchars, a struct sgttyb, a struct
   tchars, a struct ltchars, and a struct pagechars, struct
   emacs_tty should contain an element for each parameter struct
   that Emacs may change.  */


/* For each tty parameter structure that Emacs might want to save and restore,
   - include an element for it in this structure, and
   - extend the emacs_{get,set}_tty functions in sysdep.c to deal with the
     new members.  */

struct emacs_tty {

/* There is always one of the following elements, so there is no need
   for dummy get and set definitions.  */
#ifndef DOS_NT
  struct termios main;
#else /* DOS_NT */
  unsigned main;
#endif /* DOS_NT */
};

/* From sysdep.c or w32.c  */
extern int emacs_get_tty (int, struct emacs_tty *) EXTERNALLY_VISIBLE;
extern int emacs_set_tty (int, struct emacs_tty *, bool) EXTERNALLY_VISIBLE;
extern void suppress_echo_on_tty (int);
extern int serial_open (Lisp_Object);
extern void serial_configure (struct Lisp_Process *, Lisp_Object);

#endif /* EMACS_SYSTTY_H */

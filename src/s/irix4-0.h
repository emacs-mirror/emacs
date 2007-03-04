/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 4.0

   Copyright (C) 1999, 2001, 2002, 2003, 2004, 2005, 2006,
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


#include "irix3-3.h"

#define USG5_3
#define IRIX4
/* XPointer is not defined in the older X headers  -- JPff@maths.bath.ac.uk */
#define XPointer caddr_t

#undef NEED_SIOCTL

/* Include unistd.h, even though we don't define POSIX.  */
#define NEED_UNISTD_H

/* Make process_send_signal work by "typing" a signal character on the pty.  */
#define SIGNALS_VIA_CHARACTERS

#ifndef __GNUC__
/* use K&R C */
/* We need to increase the expression tree space with -Wf,-XNh
   (ghazi@caip.rutgers.edu 7/8/97.)
*/
#define C_SWITCH_SYSTEM -cckr -Wf,-XNh4000
#endif

/* SGI has all the fancy wait stuff, but we can't include sys/wait.h
   because it defines BIG_ENDIAN and LITTLE_ENDIAN (ugh!.)  Instead
   we'll just define WNOHANG right here.
   (An implicit decl is good enough for wait3.)  */

#define WNOHANG		0x1

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#undef PTY_TTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#define PTY_NAME_SPRINTF
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
/* It is necessary to prevent SIGCHLD signals within _getpty.
   So we block them. */
#define PTY_OPEN						\
{								\
  int mask = sigblock (sigmask (SIGCHLD));			\
  char *name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	\
  sigsetmask(mask);						\
  if (name == 0)						\
    return -1;							\
  if (fd < 0)							\
    return -1;							\
  if (fstat (fd, &stb) < 0)					\
    return -1;							\
  strcpy (pty_name, name);					\
}

/* arch-tag: cfd7e200-a4dc-4f67-9a32-4184c10b0c57
   (do not change this comment) */

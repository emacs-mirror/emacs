/* System description file for SCO 3.2v4.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

/* Contributed by Ian Lance Taylor, ian@cygnus.com.  */

/* SCO is sort of like SVR3.  */
#include "usg5-3.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.  */
#undef SYSTEM_TYPE
#define SYSTEM_TYPE "SCO 3.2v4"

/* SCO supports job control.  */
#undef NOMULTIPLEJOBS

/* SCO has termios.  */
#define HAVE_TERMIOS

/* SCO has timeval.  */
#define HAVE_TIMEVAL

/* SCO has ptys with unusual names.  */
#define HAVE_PTYS

#define PTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ptyp%d", ((c - FIRST_PTY_LETTER) * 16) + i);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ttyp%d", ((c - FIRST_PTY_LETTER) * 16) + i);

/* SCO has bcopy, et. al.  */
#define BSTRING

/* Use both <time.h> and <sys/time.h>.  */
#define TIME_WITH_SYS_TIME

/* Sockets are an option on SCO.  If we have X, we have them.  */
#ifdef HAVE_X_WINDOWS
#define HAVE_SOCKETS
#endif

#ifdef HAVE_SOCKETS
#define LIBS_SYSTEM -lsocket
#endif

/* We don't have -loldX, and we don't need it.  */
#define LIB_XMENU_LIB

/* SCO does have TIOCGWINSZ.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_PTEM_H

/* SCO has rename, but some people say it is broken.  Try this for
   now.  */
#define HAVE_RENAME

/* We need to link with crt1.o and crtn.o.  */
#define START_FILES pre-crt0.o /lib/crt1.o
#define LIB_STANDARD -lc /lib/crtn.o

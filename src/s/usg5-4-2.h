/* s/ file for System V release 4.2.

   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
                 2008  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include "usg5-4.h"

/* fnf@cygnus.com says these exist.  */
#define HAVE_TCATTR
#if 0 /* autoconf should take care of this.  */
#define HAVE_GETHOSTNAME
#define HAVE_RANDOM
#endif
/* #define HAVE_GETWD  (appears to be buggy on SVR4.2) */
#undef HAVE_GETWD

/* Info from fnf@cygnus.com suggests this is appropriate.  */
#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask
#undef HAVE_SYSV_SIGPAUSE

/* Motif needs -lgen.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen

/* This is the same definition as in usg5-4.h, but with sigblock/sigunblock
   rather than sighold/sigrelse, which appear to be BSD4.1 specific and won't
   work if POSIX_SIGNALS is defined.  It may also be appropriate for SVR4.x
   (x<2) but I'm not sure.   fnf@cygnus.com */
/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#undef PTY_TTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname(), *ptyname;			\
						\
    sigblock(sigmask(SIGCLD));			\
    if (grantpt(fd) == -1)			\
      fatal("could not grant slave pty");	\
    sigunblock(sigmask(SIGCLD));		\
    if (unlockpt(fd) == -1)			\
      fatal("could not unlock slave pty");	\
    if (!(ptyname = ptsname(fd)))		\
      fatal ("could not enable slave pty");	\
    strncpy(pty_name, ptyname, sizeof(pty_name)); \
    pty_name[sizeof(pty_name) - 1] = 0;		\
  }

/* Use libw.a along with X11R6 Xt.  */
#define NEED_LIBW

/* ryanr@ellingtn.ftc.nrcs.usda.gov (Richard Anthony Ryan) says -lXimp
   is needed in UNIX_SV ... 4.2 1.1.2.  */
#define LIB_MOTIF -lXm -lXimp

/* arch-tag: 9bbfcfc1-19be-45a1-9699-af57b87da2c6
   (do not change this comment) */

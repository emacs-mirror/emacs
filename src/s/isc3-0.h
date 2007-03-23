/* s- file for Interactive (ISC) Unix version 3.0 on the 386.

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


#include "isc2-2.h"

/* This has been moved into isc2-2.h.  */
/* #define HAVE_SOCKETS */

/* This appears on 3.0, presumably as part of what SunSoft call X2. */
#undef NO_X_DESTROY_DATABASE

#ifdef __GNUC__  /* Currently we use -lcposix only with gcc */
#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask
#undef HAVE_SYSV_SIGPAUSE
#endif

/* People say that using -traditional causes lossage with `const',
   so we might as well try getting rid of -traditional.  */
#undef C_SWITCH_SYSTEM

/* We indirectly #include s/usg5-3.h, which says to use libX11_s and
   libc_s.  Martin Tomes <mt00@controls.eurotherm.co.uk> says that ISC
   has no libX11_s, and that linking with libc_s causes sbrk not to work.  */
#undef LIB_X11_LIB
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt -lnls -lnsl_s

/* TIOCGWINSZ isn't broken; you just have to know where to find it.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_SIOCTL

/* We need either _XOPEN_SOURCE or _POSIX_SOURCE to import the posix
   signal symbols; might as well use _XOPEN_SOURCE.  Defining _SYSV3
   ensures that we don't lose the traditional symbols as a side effect
   from this or __STDC__ being defined.  It can't hurt to Define
   _XOPEN_SOURCE=500, the latest and greatest value as of this writing.  */
#define C_SWITCH_SYSTEM -D_XOPEN_SOURCE=500 -D_SYSV3

#ifdef __GNUC__  /* Currently we use -lcposix only with gcc */
/* This works around a bug in ISC 4.0 and 3.0; it fails
   to clear the "POSIX process" flag on an exec.
   It won't be needed for 4.1.  */
#define EXTRA_INITIALIZE __setostype (0)
#endif

/* arch-tag: c1aca3f2-813d-4c1c-ad64-ca6c20ec9bfb
   (do not change this comment) */

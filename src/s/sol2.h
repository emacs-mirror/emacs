/* Definitions file for GNU Emacs running on Solaris 2.
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

#define SOLARIS2

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  */
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lkstat

/* Prefer kstat over kvm in getloadavg.c, kstat doesn't require root.
   ghazi@caip.rutgers.edu, 7/21/97.  Don't redefine if already defined
   (e.g., by config.h). */
#ifndef HAVE_LIBKSTAT
#define HAVE_LIBKSTAT
#endif

/* eggert thinks all versions of SunPro C allowed this.  */
#ifndef __GNUC__
#define C_DEBUG_SWITCH -g -O
#endif

/* inoue@ainet.or.jp says Solaris has a bug related to X11R6-style
   XIM support.  */

#define INHIBIT_X11R6_XIM

/* Must use the system's termcap, if we use any termcap.
   It does special things.  */

#ifndef TERMINFO
#define LIBS_TERMCAP -ltermcap
#endif

#define USE_MMAP_FOR_BUFFERS 1

/* arch-tag: b0640f78-5ad5-4093-97c3-5b3abbf5a2be
   (do not change this comment) */

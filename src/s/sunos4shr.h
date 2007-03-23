/*
Copyright (C) 1994, 2001, 2002, 2003, 2004, 2005, 2006,
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


/* This file permits building Emacs with a shared libc on Sunos 4.
   To make this work, you must completely replace your C shared library
   using one of the SunOS 4.1.x jumbo replacement patches from Sun.
   Here are the patch numbers for Sunos 4.1.3:
   100890-10   SunOS 4.1.3: domestic libc jumbo patch
   100891-10   SunOS 4.1.3: international libc jumbo patch  */


#include "sunos4-1.h"

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

/*  Misleading!  Actually gets loaded after crt0.o */
#define START_FILES pre-crt0.o

/*
 *  Kludge!  can't get at symbol "start" in std crt0.o
 *  Who the #$%&* decided to remove the __ characters!
 *  Someone needs to fix this in sysdep.c  with an #ifdef BROKEN_START in
 * sysdep.c.  We do not use this address so any value should do really.  Still
 *  may need it in the future?
 */
#define BROKEN_START
#define TEXT_START 0x2020

#define UNEXEC	unexsunos4.o
#define RUN_TIME_REMAP
#define ORDINARY_LINK
#define SUNOS4_SHARED_LIBRARIES

#undef LD_SWITCH_SYSTEM
#undef LD_SWITCH_SYSTEM_TEMACS

#undef	SYSTEM_MALLOC
#ifndef GNU_MALLOC
#define	GNU_MALLOC
#endif
#ifndef REL_ALLOC
#define	REL_ALLOC
#endif

/* khera@cs.duke.edu says this is needed.  */
#define memmove(to, from, size) bcopy (from, to, size)

#undef USE_DL_STUBS

#ifndef HAVE_X11R6
/* With X11R5 it was reported that linking -lXmu dynamically
   did not work.  With X11R6, it does work; and since normally
   only the dynamic libraries are available, we should use them.  */
#ifdef __GNUC__
#define LIBXMU -Xlinker -Bstatic -lXmu -Xlinker -Bdynamic
#else
#define LIBXMU -Bstatic -lXmu -Bdynamic
#endif

#endif  /* not HAVE_X11R6 */

/* arch-tag: cb54321a-ed45-4c17-a23e-1c157758da78
   (do not change this comment) */

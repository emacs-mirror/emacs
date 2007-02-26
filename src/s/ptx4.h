/* s/ file for Sequent "ptx 4", which is a modified SVR5.4.

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
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


/* Tell usg5-4.h not to include filio.h.  */
#define NO_FILIO_H

#include "usg5-4.h"

/* Marcus Daniels <marcus@sysc.pdx.edu> says that SIGINFO is defined
   on ptx4 but it is not a signal.  Prevent process.c from doing the
   wrong thing.  */
#define BROKEN_SIGINFO

/* pae@dim.com (Phil Ernhardt) says this correction to
   the definition in usg5-4.h is needed to prevent
   all asynchronous subprocesses from exiting right away.  */

/* James Youngman <jay@gnu.org> found that on "DYNIX/ptx ARNIE 4.0
 * V4.4.2 i386", the push of the ttcompat module would fail.  It seems
 * that PTX 4.4.2 doesn't have that module, so if the push fails we
 * don't kill ourselves.  While this version lacks ttcompat, it also
 * has ptem, but the manual page for ptem indicates that it should
 * be pushed onto the slave side before the line discipline module.
 * See also the streampty manual page, if you're curious (and have
 * a ptx system).
 */

/* rms: I hope that older versions which do have ttcompat
   will not get confused by the code to use ptem.  */

#undef SETUP_SLAVE_PTY
#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1 && errno != EINVAL) \
    fatal ("ioctl I_PUSH ptem", errno);     \
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1 && errno != EINVAL) \
    fatal ("ioctl I_PUSH ttcompat", errno);

/* arch-tag: 14621824-8dca-432b-a97a-049fc4ce0e9f
   (do not change this comment) */

/* sysselect.h - System-dependent definitions for the select function.
   Copyright (C) 1995, 2001-2020 Free Software Foundation, Inc.

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

#ifndef SYSSELECT_H
#define SYSSELECT_H 1

#ifndef DOS_NT
#include <sys/select.h>
#endif

#include "lisp.h"

/* The w32 build defines select stuff in w32.h, which is included
   where w32 needs it, but not where sysselect.h is included.  The w32
   definitions in w32.h are incompatible with the below.  */
#ifndef WINDOWSNT
#ifdef FD_SET
#ifndef FD_SETSIZE
#define FD_SETSIZE 64
#endif
#else /* no FD_SET */
#define FD_SETSIZE 32
typedef int fd_set;

/* Define the macros to access a single-int bitmap of descriptors.  */
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)
#endif /* no FD_SET */
#endif /* not WINDOWSNT */

#if !defined (HAVE_SELECT)
#define select sys_select
#endif

#ifdef MSDOS
/* The above #define for 'select' gets in the way because sysselect.h
   is included in thread.h, which is included everywhere, and 'select'
   declared in DJGPP system headers has a signature incompatible with
   'pselect', which we emulate in msdos.c.  */
#undef select
#define pselect sys_select
#endif

#ifndef WINDOWSNT
INLINE_HEADER_BEGIN

/* Check for out-of-range errors if ENABLE_CHECKING is defined.  */

INLINE void
fd_CLR (int fd, fd_set *set)
{
  eassume (0 <= fd && fd < FD_SETSIZE);
  FD_CLR (fd, set);
}

INLINE bool
fd_ISSET (int fd, fd_set *set)
{
  eassume (0 <= fd && fd < FD_SETSIZE);
  return FD_ISSET (fd, set) != 0;
}

INLINE void
fd_SET (int fd, fd_set *set)
{
  eassume (0 <= fd && fd < FD_SETSIZE);
  FD_SET (fd, set);
}

#undef FD_CLR
#undef FD_ISSET
#undef FD_SET
#define FD_CLR(fd, set) fd_CLR (fd, set)
#define FD_ISSET(fd, set) fd_ISSET (fd, set)
#define FD_SET(fd, set) fd_SET (fd, set)

INLINE_HEADER_END

#endif	/* !WINDOWSNT */

#endif

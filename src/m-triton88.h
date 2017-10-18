/* m- file for Motorola System V/88 machines
   Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m-delta88.h"

#ifndef TRITON88
#define TRITON88
#endif /* TRITON88 */

/* I don't think Trinto88 needs this. */

#ifdef BROKEN_FIONREAD
#undef BROKEN_FIONREAD
#endif /* BROKEN_FIONREAD */

/* Need this to prevent Ctrl-Z from suspending Emacs before suspend-emacs
   have been called. */

#define HAVE_TCATTR

/* libc defines the following functions. */

#define HAVE_RENAME
#define HAVE_CLOSEDIR
#define HAVE_DUP2
#define HAVE_SETSID

/* Have to this because of the brain damaged malloc in libc on Triton88
   machines. */

#define C_SWITCH_SYSTEM -Dmalloc=_malloc -Dfree=_free  -Drealloc=_realloc -Dcalloc=_calloc

/* There is no termio.h.  */

#define NO_TERMIO

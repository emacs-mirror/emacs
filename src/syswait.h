/* Define wait system call interface for Emacs.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

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

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifndef VMS
#ifndef WAITTYPE

#ifdef WAIT_USE_INT
/* Some systems have  union wait  in their header, but we should use
   int regardless of that.  */
#include <sys/wait.h>
#define WAITTYPE int
#define WRETCODE(w) WEXITSTATUS (w)

#else /* not WAIT_USE_INT */

#if (!defined (BSD) && !defined (UNIPLUS) && !defined (STRIDE) && !(defined (HPUX) && !defined (NOMULTIPLEJOBS)) && !defined (HAVE_WAIT_HEADER)) || defined (LINUX)
#define WAITTYPE int
#define WIFSTOPPED(w) ((w&0377) == 0177)
#define WIFSIGNALED(w) ((w&0377) != 0177 && (w&~0377) == 0)
#define WIFEXITED(w) ((w&0377) == 0)
#define WRETCODE(w) (w >> 8)
#define WSTOPSIG(w) (w >> 8)
#define WTERMSIG(w) (w & 0377)
#ifndef WCOREDUMP
#define WCOREDUMP(w) ((w&0200) != 0)
#endif

#else 

#ifdef BSD4_1
#include <wait.h>
#else
#include <sys/wait.h>
#endif /* not BSD 4.1 */

#define WAITTYPE union wait
#define WRETCODE(w) w.w_retcode
#undef WCOREDUMP		/* Later BSDs define this name differently.  */
#define WCOREDUMP(w) w.w_coredump

#if defined (HPUX) || defined (convex)
/* HPUX version 7 has broken definitions of these.  */
/* pvogel@convex.com says the convex does too.  */
#undef WTERMSIG
#undef WSTOPSIG
#undef WIFSTOPPED
#undef WIFSIGNALED
#undef WIFEXITED
#endif /* HPUX | convex */

#ifndef WTERMSIG
#define WTERMSIG(w) w.w_termsig
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(w) w.w_stopsig
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(w) (WTERMSIG (w) == 0177)
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(w) (WTERMSIG (w) != 0177 && (WSTOPSIG (w)) == 0)
#endif
#ifndef WIFEXITED
#define WIFEXITED(w) (WTERMSIG (w) == 0)
#endif
#endif /* BSD or UNIPLUS or STRIDE */
#endif /* not WAIT_USE_INT */
#endif /* no WAITTYPE */

#else /* VMS */

#define WAITTYPE int
#define WIFSTOPPED(w) 0
#define WIFSIGNALED(w) 0
#define WIFEXITED(w) ((w) != -1)
#define WRETCODE(w) (w)
#define WSTOPSIG(w) (w)
#define WCOREDUMP(w) 0
#define WTERMSIG(w) (w)
#include <ssdef.h>
#include <iodef.h>
#include <clidef.h>
#include "vmsproc.h"

#endif /* VMS */

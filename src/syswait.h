/* Define wait system call interface for Emacs.
   Copyright (C) 1993, 1994, 1995, 2000, 2001, 2002, 2003, 2004,
                 2005, 2006, 2007, 2008  Free Software Foundation, Inc.

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

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifndef EMACS_SYSWAIT_H
#define EMACS_SYSWAIT_H

#ifndef VMS

/* This is now really the approach recommended by Autoconf.  If this
   doesn't cause trouble anywhere, remove the original code, which is
   #if'd out below.  */

#if 1
#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H	/* We have sys/wait.h with POSIXoid definitions. */
#include <sys/wait.h>
#endif  /* !HAVE_SYS_WAIT_H */

#ifndef WCOREDUMP		/* not POSIX */
#define WCOREDUMP(status) ((status) & 0x80)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(status) (((status)  & 0xff00) >> 8)
#endif
#ifndef WIFEXITED
#define WIFEXITED(status) (WTERMSIG(status) == 0)
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(status) (((status) & 0xff) == 0x7f)
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(status) (!WIFSTOPPED(status) && !WIFEXITED(status))
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(status) WEXITSTATUS(status)
#endif
#ifndef WTERMSIG
#define WTERMSIG(status) ((status) & 0x7f)
#endif

#undef WAITTYPE
#define WAITTYPE int
#undef WRETCODE
#define WRETCODE(status) WEXITSTATUS (status)

#else  /* 0 */

#ifndef WAITTYPE

#ifdef WAIT_USE_INT
/* Some systems have  union wait  in their header, but we should use
   int regardless of that.  */
#include <sys/wait.h>
#define WAITTYPE int
#define WRETCODE(w) WEXITSTATUS (w)

#else /* not WAIT_USE_INT */

#if (!defined (BSD_SYSTEM) && !(defined (HPUX) && !defined (NOMULTIPLEJOBS)) && !defined (HAVE_WAIT_HEADER))
#define WAITTYPE int
#define WIFSTOPPED(w) ((w&0377) == 0177)
#define WIFSIGNALED(w) ((w&0377) != 0177 && (w&~0377) == 0)
#define WIFEXITED(w) ((w&0377) == 0)
#define WRETCODE(w) (w >> 8)
#define WSTOPSIG(w) (w >> 8)
#define WTERMSIG(w) (w & 0177)
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
#endif /* BSD_SYSTEM || HPUX */
#endif /* not WAIT_USE_INT */
#endif /* no WAITTYPE */

#endif /* 0 */

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

#endif /* EMACS_SYSWAIT_H */

/* arch-tag: 7e5d9719-ec66-4b6f-89bb-563eea16a899
   (do not change this comment) */

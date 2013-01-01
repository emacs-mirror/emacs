/* syssignal.h - System-dependent definitions for signals.

Copyright (C) 1993, 1999, 2001-2013 Free Software Foundation, Inc.

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

#include <signal.h>
#include <stdbool.h>

extern void init_signals (bool);

#ifdef HAVE_PTHREAD
#include <pthread.h>
/* If defined, asynchronous signals delivered to a non-main thread are
   forwarded to the main thread.  */
#define FORWARD_SIGNAL_TO_MAIN_THREAD
#endif

#if defined HAVE_TIMER_SETTIME && defined SIGEV_SIGNAL
# define HAVE_ITIMERSPEC
#endif

#if (defined SIGPROF && !defined PROFILING \
     && (defined HAVE_SETITIMER || defined HAVE_ITIMERSPEC))
# define PROFILER_CPU_SUPPORT
#endif

extern sigset_t empty_mask;

typedef void (*signal_handler_t) (int);

extern void emacs_sigaction_init (struct sigaction *, signal_handler_t);
char const *safe_strsignal (int) ATTRIBUTE_CONST;

#if NSIG < NSIG_MINIMUM
# undef NSIG
# define NSIG NSIG_MINIMUM
#endif

#ifndef emacs_raise
# define emacs_raise(sig) raise (sig)
#endif

/* On bsd, [man says] kill does not accept a negative number to kill a pgrp.
   Must do that using the killpg call.  */
#ifdef BSD_SYSTEM
#define EMACS_KILLPG(gid, signo) (killpg ( (gid), (signo)))
#else
#ifdef WINDOWSNT
#define EMACS_KILLPG(gid, signo) (kill (gid, signo))
#else
#define EMACS_KILLPG(gid, signo) (kill   (-(gid), (signo)))
#endif
#endif

/* Define SIGCHLD as an alias for SIGCLD.  There are many conditionals
   testing SIGCHLD.  */
#ifdef SIGCLD
#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif /* SIGCHLD */
#endif /* ! defined (SIGCLD) */

#ifndef HAVE_STRSIGNAL
# define strsignal(sig) safe_strsignal (sig)
#endif

void deliver_process_signal (int, signal_handler_t);

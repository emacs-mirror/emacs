/* syssignal.h - System-dependent definitions for signals.

Copyright (C) 1993, 1999, 2001-2012 Free Software Foundation, Inc.

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

extern void init_signals (void);

#ifdef HAVE_PTHREAD
#include <pthread.h>
/* If defined, asynchronous signals delivered to a non-main thread are
   forwarded to the main thread.  */
#define FORWARD_SIGNAL_TO_MAIN_THREAD
#endif

extern sigset_t empty_mask;

typedef void (*signal_handler_t) (int);

extern void emacs_sigaction_init (struct sigaction *, signal_handler_t);

#if ! (defined TIOCNOTTY || defined USG5 || defined CYGWIN)
_Noreturn void croak (char *);
#endif

/* Interrupt input is not used if there is no FIONREAD.  */
#include <sys/ioctl.h>
#if defined BROKEN_SIGIO || ! defined FIONREAD || defined BROKEN_FIONREAD
# undef SIGIO
#endif

/* These are only used by AIX  */
#if defined (SIGPOLL) && defined (BROKEN_SIGPOLL)
#undef SIGPOLL
#endif
#if defined (SIGAIO) && defined (BROKEN_SIGAIO)
#undef SIGAIO
#endif
#if defined (SIGPTY) && defined (BROKEN_SIGPTY)
#undef SIGPTY
#endif

#if NSIG < NSIG_MINIMUM
# undef NSIG
# define NSIG NSIG_MINIMUM
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
/* strsignal is in sysdep.c */
char *strsignal (int);
#endif

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
extern pthread_t main_thread;
#endif

void handle_on_main_thread (int, signal_handler_t);

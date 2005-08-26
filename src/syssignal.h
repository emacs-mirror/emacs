/* syssignal.h - System-dependent definitions for signals.
   Copyright (C) 1993, 1999, 2002, 2003, 2004,
                 2005 Free Software Foundation, Inc.

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

extern void init_signals P_ ((void));

#ifdef HAVE_GTK_AND_PTHREAD
#include <pthread.h>
extern pthread_t main_thread;
#endif

#ifdef POSIX_SIGNALS

/* Don't #include <signal.h>.  That header should always be #included
   before "config.h", because some configuration files (like s/hpux.h)
   indicate that SIGIO doesn't work by #undef-ing SIGIO.  If this file
   #includes <signal.h>, then that will re-#define SIGIO and confuse
   things.  */

#define SIGMASKTYPE sigset_t

#define SIGEMPTYMASK (empty_mask)
#define SIGFULLMASK (full_mask)
extern sigset_t empty_mask, full_mask;

/* POSIX pretty much destroys any possibility of writing sigmask as a
   macro in standard C.  We always define our own version because the
   predefined macro in Glibc 2.1 is only provided for compatility for old
   programs that use int as signal mask type.  */
#undef sigmask
#ifdef __GNUC__
#define sigmask(SIG) 				\
  ({						\
    sigset_t _mask;				\
    sigemptyset (&_mask);			\
    sigaddset (&_mask, SIG);			\
    _mask;					\
  })
#else /* ! defined (__GNUC__) */
extern sigset_t sys_sigmask ();
#define sigmask(SIG) (sys_sigmask (SIG))
#endif /* ! defined (__GNUC__) */

#undef sigpause
#define sigpause(MASK)    sigsuspend (&(MASK))

#define sigblock(SIG)    sys_sigblock (SIG)
#define sigunblock(SIG)  sys_sigunblock (SIG)
#ifndef sigsetmask
#define sigsetmask(SIG)  sys_sigsetmask (SIG)
#endif
#define sighold(SIG)     ONLY_USED_IN_BSD_4_1
#define sigrelse(SIG)    ONLY_USED_IN_BSD_4_1
#undef signal
#define signal(SIG,ACT)      sys_signal(SIG,ACT)

/* Whether this is what all systems want or not, this is what
   appears to be assumed in the source, for example data.c:arith_error.  */
typedef RETSIGTYPE (*signal_handler_t) (/*int*/);

signal_handler_t sys_signal P_ ((int signal_number, signal_handler_t action));
sigset_t sys_sigblock   P_ ((sigset_t new_mask));
sigset_t sys_sigunblock P_ ((sigset_t new_mask));
sigset_t sys_sigsetmask P_ ((sigset_t new_mask));

#define sys_sigdel(MASK,SIG) sigdelset (&MASK,SIG)

#else /* ! defined (POSIX_SIGNALS) */
#ifdef USG5_4

extern SIGMASKTYPE sigprocmask_set;

#ifndef sigblock
#define sigblock(sig)					\
     (sigprocmask_set = SIGEMPTYMASK | (sig),		\
      sigprocmask (SIG_BLOCK, &sigprocmask_set, NULL))
#endif

#ifndef sigunblock
#define sigunblock(sig)						\
     (sigprocmask_set = SIGFULLMASK & ~(sig),			\
      sigprocmask (SIG_SETMASK, &sigprocmask_set, NULL))
#endif

#else
#ifdef USG

#ifndef sigunblock
#define sigunblock(sig)
#endif

#else

#ifndef sigunblock
#define sigunblock(SIG) \
{ SIGMASKTYPE omask = sigblock (SIGEMPTYMASK); sigsetmask (omask & ~SIG); }
#endif

#endif /* ! defined (USG) */
#endif /* ! defined (USG5_4) */
#endif /* ! defined (POSIX_SIGNALS) */

#ifndef SIGMASKTYPE
#define SIGMASKTYPE int
#endif

#ifndef SIGEMPTYMASK
#define SIGEMPTYMASK (0)
#endif

#ifndef SIGFULLMASK
#define SIGFULLMASK (0xffffffff)
#endif

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#ifndef sigunblock
#define sigunblock(SIG) \
{ SIGMASKTYPE omask = sigblock (SIGFULLMASK); sigsetmask (omask & ~SIG); }
#endif

#ifndef BSD4_1
#define sigfree() sigsetmask (SIGEMPTYMASK)
#endif /* not BSD4_1 */

#if defined (SIGINFO) && defined (BROKEN_SIGINFO)
#undef SIGINFO
#endif
#if defined (SIGIO) && defined (BROKEN_SIGIO)
#undef SIGIO
#endif
#if defined (SIGPOLL) && defined (BROKEN_SIGPOLL)
#undef SIGPOLL
#endif
#if defined (SIGTSTP) && defined (BROKEN_SIGTSTP)
#undef SIGTSTP
#endif
#if defined (SIGURG) && defined (BROKEN_SIGURG)
#undef SIGURG
#endif
#if defined (SIGAIO) && defined (BROKEN_SIGAIO)
#undef SIGAIO
#endif
#if defined (SIGPTY) && defined (BROKEN_SIGPTY)
#undef SIGPTY
#endif


#if NSIG < NSIG_MINIMUM
# ifdef NSIG
#  undef NSIG
# endif
# define NSIG NSIG_MINIMUM
#endif

#ifdef BSD4_1
#define SIGIO SIGTINT
/* sigfree is in sysdep.c */
#endif /* BSD4_1 */

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
#ifndef VMS
#ifdef SIGCLD
#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif /* SIGCHLD */
#endif /* ! defined (SIGCLD) */
#endif /* VMS */

#ifndef HAVE_STRSIGNAL
/* strsignal is in sysdep.c */
char *strsignal ();
#endif

#ifdef HAVE_GTK_AND_PTHREAD
#define SIGNAL_THREAD_CHECK(signo)                                      \
  do {                                                                  \
    if (pthread_self () != main_thread)                                 \
      {                                                                 \
        /* POSIX says any thread can receive the signal.  On GNU/Linux  \
           that is not true, but for other systems (FreeBSD at least)   \
           it is.  So direct the signal to the correct thread and block \
           it from this thread.  */                                     \
        sigset_t new_mask;                                              \
                                                                        \
        sigemptyset (&new_mask);                                        \
        sigaddset (&new_mask, signo);                                   \
        pthread_sigmask (SIG_BLOCK, &new_mask, 0);                      \
        pthread_kill (main_thread, signo);                              \
        return;                                                         \
      }                                                                 \
   } while (0)

#else /* not HAVE_GTK_AND_PTHREAD */
#define SIGNAL_THREAD_CHECK(signo)
#endif /* not HAVE_GTK_AND_PTHREAD */
/* arch-tag: 4580e86a-340d-4574-9e11-a742b6e1a152
   (do not change this comment) */

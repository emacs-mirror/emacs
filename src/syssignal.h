/* syssignal.h - System-dependent definitions for signals.
   Copyright (C) 1992 Free Software Foundation, Inc.

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


#ifdef POSIX_SIGNALS
#define SIGMASKTYPE sigset_t

#define SIGEMPTYMASK (empty_mask)
#define SIGFULLMASK (full_mask)
extern sigset_t empty_mask, full_mask, temp_mask;

#define sigmask(SIG) \
(sigemptyset (&temp_mask), sigaddset (&temp_mask, SIG), temp_mask)

/* The below routines may need a local mask.  There could be problems
   if code using any of the 3 macros below could be reentered due to a
   signal occurring.  This can't happen in Emacs 18.57, so we don't
   worry. - DJB */

#define EMACS_SIGPAUSE(sigset) \
  do { sigset_t _mask; sys_sigpause (sigset); } while (0)
#define EMACS_SIGBLOCK(new_sig, old_sig) \
  do { sigset_t _mask; (old_sig) = sys_sigblock (new_sig); } while (0)
#define EMACS_SIGUNBLOCK(new_sig, old_sig) \
  do { sigset_t _mask; (old_sig) = sys_sigunblock (new_sig); } while (0)
#define EMACS_SIGSETMASK(new_sig, old_sig) \
  do { sigset_t _mask; (old_sig) = sys_sigsetmask (new_sig); } while (0)
#define sighold(SIG)     ONLY_USED_IN_BSD_4_1
#define sigrelse(SIG)    ONLY_USED_IN_BSD_4_1

int (*sys_signal (int signal_number, int (*action)())) ();
int sys_sigpause (int signal_number);
sigset_t sys_sigblock (sigset_t new_mask);
sigset_t sys_sigunblock (sigset_t new_mask);
sigset_t sys_sigsetmask (sigset_t new_mask);

#define sys_sigdel(MASK,SIG) sigdelset(&MASK,SIG)

#else /* ! defined (POSIX_SIGNALS) */

#define sigunblock(SIG) \
{ SIGMASKTYPE omask = sigblock (SIGEMPTYMASK); sigsetmask (omask & ~SIG); }

#endif /* ! defined (POSIX_SIGNALS) */

#ifndef SIGMASKTYPE
#define SIGMASKTYPE int
#endif

#ifndef SIGEMPTYMASK
#define SIGEMPTYMASK 0
#endif

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#ifdef BSD4_1
#define SIGIO SIGTINT
/* sigfree and sigholdx are in sysdep.c */
#define EMACS_SIGFREE () sigfree ()

/* We define the following macros to expand into statements rather
   than expressions, because the POSIX macros above do the same, and
   we don't want people on BSD4_1 systems accidentally using the
   macros in a way that will break the other systems.  */
#define EMACS_SIGHOLDX(new_sig, old_sig) \
  do { (old_sig) = sigholdx (new_sig); } while (0)
#define EMACS_SIGBLOCKX(new_sig, old_sig) \
  do { (old_sig) = sighold (new_sig); } while (0)
#define EMACS_SIGUNBLOCKX(new_sig, old_sig) \
  do { (old_sig) = sigrelse (new_sig); } while (0)
#define EMACS_SIGPAUSEX(sig) \
  EMACS_SIGPAUSE (new_sig);

#else /* ! defined (BSD4_1) */

#define EMACS_SIGFREE() \
  do { SIGMASKTYPE _dummy; EMACS_SIGSETMASK (SIGEMPTYMASK, _dummy); } while (0)
#define EMACS_SIGHOLDX(new_sig, old_sig) \
  EMACS_SIGSETMASK (sigmask (new_sig), old_sig)
#define EMACS_SIGBLOCKX(new_sig, old_sig) \
  EMACS_SIGBLOCK (sigmask (new_sig), old_sig)
#define EMACS_SIGUNBLOCKX(new_sig, old_sig) \
  EMACS_SIGUNBLOCK (sigmask (new_sig), old_sig)
#define EMACS_SIGPAUSEX(sig) \
  EMACS_SIGPAUSE (0)

#endif /* ! defined (BSD4_1) */

/* On bsd, [man says] kill does not accept a negative number to kill a pgrp.
   Must do that using the killpg call.  */
#ifdef BSD
#define EMACS_KILLPG(gid, signo) (killpg ( (gid), (signo)))
#else
#define EMACS_KILLPG(gid, signo) (kill   (-(gid), (signo)))
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

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

/* May need a local mask.  There could be problems if code using any
   of the 3 macros below could be reentered due to a signal occurring.
   This can't happen in Emacs 18.57, so we don't worry. - DJB
   These macros also require GCC. */
#define sigpause(SIG)    ({ sigset_t _mask; sys_sigpause(SIG); })
#define sigblock(SIG)    ({ sigset_t _mask; sys_sigblock(SIG); })
#define sigunblock(SIG)  ({ sigset_t _mask; sys_sigunblock(SIG); })
#define sigsetmask(SIG)  ({ sigset_t _mask; sys_sigsetmask(SIG); })
#define sighold(SIG)     ONLY_USED_IN_BSD_4_1
#define sigrelse(SIG)    ONLY_USED_IN_BSD_4_1

int (*sys_signal (int signal_number, int (*action)())) ();
int sys_sigpause (int signal_number);
sigset_t sys_sigblock (sigset_t new_mask);
sigset_t sys_sigunblock (sigset_t new_mask);
sigset_t sys_sigsetmask (sigset_t new_mask);

#define sys_sigdel(MASK,SIG) sigdelset(&MASK,SIG)

#else /* not POSIX_SIGNALS */

#define sigunblock(SIG) \
{ SIGMASKTYPE omask = sigblock (SIGEMPTYMASK); sigsetmask (omask & ~SIG); }

#endif /* not POSIX_SIGNALS */

#ifndef SIGMASKTYPE
#define SIGMASKTYPE int
#endif

#ifndef SIGEMPTYMASK
#define SIGEMPTYMASK 0
#endif

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#ifndef BSD4_1
#define sigfree() sigsetmask (SIGEMPTYMASK)
#define sigholdx(sig) sigsetmask (sigmask (sig))
#define sigblockx(sig) sigblock (sigmask (sig))
#define sigunblockx(sig) sigblock (SIGEMPTYMASK)
#define sigpausex(sig) sigpause (0)
#endif /* not BSD4_1 */

#ifdef BSD4_1
#define SIGIO SIGTINT
/* sigfree and sigholdx are in sysdep.c */
#define sigblockx(sig) sighold (sig)
#define sigunblockx(sig) sigrelse (sig)
#define sigpausex(sig) sigpause (sig)
#endif /* BSD4_1 */

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
#endif /* not SIGCHLD */
#endif /* SIGCLD */
#endif /* not VMS */

#include "sol2.h"

/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO

/* Override LD_SWITCH_SYSTEM: add  -L /usr/ccs/lib to the sol2.h value.  */

#undef LD_SWITCH_SYSTEM

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */

/* Info from fnf@cygnus.com suggests this is appropriate.  */
#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask

/* This is the same definition as in usg5-4.h, but with sigblock/sigunblock
   rather than sighold/sigrelse, which appear to be BSD4.1 specific and won't
   work if POSIX_SIGNALS is defined.  It may also be appropriate for SVR4.x
   (x<2) but I'm not sure.   fnf@cygnus.com */
/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#undef PTY_TTY_NAME_SPRINTF
#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname(), *ptyname;			\
						\
    sigblock(sigmask(SIGCLD));			\
    if (grantpt(fd) == -1)			\
      fatal("could not grant slave pty");	\
    sigunblock(sigmask(SIGCLD));		\
    if (unlockpt(fd) == -1)			\
      fatal("could not unlock slave pty");	\
    if (!(ptyname = ptsname(fd)))		\
      fatal ("could not enable slave pty");	\
    strncpy(pty_name, ptyname, sizeof(pty_name)); \
    pty_name[sizeof(pty_name) - 1] = 0;		\
  }

/* David Miller <davem@caip.rutgers.edu> says vfork fails on 2.4.  */
/* Brendan Kehoe <brendan@zen.org> says it also fails on 2.3.  */
#undef HAVE_VFORK

/* Support SCO V 3.2.4 (also called Open Desk Top 2.0) */

#include "s-usg5-3.h"

/* Use termios instead of termio, to turn off C-z.  */

#define HAVE_TCATTR

/* It's possible for Emcs to suspend itself.  */

#undef NOMULTIPLEJOBS

/* The setsid system call exists.  */

#define HAVE_SETSID

/* The rename system call exists.  */

#define HAVE_RENAME

/* Include ptem.h, not sioctl.h.  */

#define NO_SIOCTL_H
#define NEED_PTEM_H

/* Inhibit macro definition of `signal' in m-intel386.h.  */

#define DONT_DEFINE_SIGNAL

/* We cannot get alloca from -lPW because various other symbols
   in that library conflict with symbols in GCC.  */

#ifdef	__GNUC__
#define alloca(x) __builtin_alloca (x)
#else
#define C_ALLOCA
#undef HAVE_ALLOCA
#define STACK_DIRECTION	-1
#endif /* __GNUC__ */

/* TIOCWINSZ doesn't work on ptys.  */
#define BROKEN_TIOCGWINSZ

/* jbc@cunixa.cc.columbia.edu says this causes compilation errors
   on SCO V.3.2.2.  */
/* #define HAVE_TIMEVAL */

#define HAVE_SELECT

#define HAVE_PTYS

/* chip@tct.com says ptys are numbered decimally.  */
#define PTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ptyp%d", ((c - FIRST_PTY_LETTER) * 16) + i);
#define PTY_TTY_NAME_SPRINTF \
  sprintf (pty_name, "/dev/ttyp%d", ((c - FIRST_PTY_LETTER) * 16) + i);

/* Christoph Badura <bad@flatlin.ka.sub.org> says this one is ok.  */
#define USG_SYS_TIME

/* Define SCO_SOCKETS if your machie has sockets installed.
   It is an optional feature in the SCO system.  */

#ifdef SCO_SOCKETS
#define HAVE_SOCKETS
#define LIBS_SYSTEM -lsocket
#define HAVE_GETTIMEOFDAY
#define HAVE_TIMEVAL
#define USE_UTIME
#endif

/* These have been suggested, but it's not certain they are right.  */
#if 0
#define HAVE_RANDOM

#define USE_UTIME
#endif /* 0 */


#ifdef HAVE_X11
/* William Smith (wjs@wang.com) said these are needed.  */
#ifdef LIBS_SYSTEM	/* undef if defined in SCO_SOCKETS */
#undef LIBS_SYSTEM
#endif
#define LIB_X11_LIB	/* kill internal libs (use one below) */
#define LIBS_SYSTEM -lXm -lXt -lX11 -lsocket -lmalloc -lPW
#define LIB_STANDARD -lc /* get libPW.a from above */

#undef C_ALLOCA
#define HAVE_ALLOCA

#define BROKEN_FIONREAD
#define SYSV_STREAMS

#define select XSelect
#define HAVE_RANDOM
#define BSTRING
#endif

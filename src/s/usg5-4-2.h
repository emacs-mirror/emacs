/* s/ file for System V release 4.2.  */

#include "usg5-4.h"

/* pcg@aber.ac.uk says this is useless since fork does copy-on-write
   #define HAVE_VFORK */
/* fnf@cygnus.com says these exist.  */
#define HAVE_TCATTR
#if 0 /* autoconf should take care of this.  */
#define HAVE_GETHOSTNAME
#define HAVE_RANDOM
#endif
/* #define HAVE_GETWD  (appears to be buggy on SVR4.2) */

/* Info from fnf@cygnus.com suggests this is appropriate.  */
#define POSIX_SIGNALS

/* We don't need the definition from usg5-3.h with POSIX_SIGNALS.  */
#undef sigsetmask
#undef HAVE_SYSV_SIGPAUSE

/* Motif needs -lgen.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM -lsocket -lnsl -lelf -lgen

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

/* Use libw.a along with X11R6 Xt.  */
#define NEED_LIBW

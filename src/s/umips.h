/* Definitions file for GNU Emacs running on Mips operating system.
   That system can emulate either BSD or Sys V, in either case with changes.
   If BSD_SYSTEM is defined, we assume BSD is being emulated; otherwise,
   Sys V.  */

#ifdef BSD_SYSTEM
#include "bsd4-3.h"

#define C_SWITCH_SYSTEM -systype bsd43
#define LD_SWITCH_SYSTEM -systype bsd43
#define LIBS_SYSTEM -lmld
#define LIBS_DEBUG
#define START_FILES pre-crt0.o /lib/crt1.o
#define LIB_STANDARD -lc /usr/lib/crtn.o

#define COFF
#define TERMINFO
#undef MAIL_USE_FLOCK  /* Someone should check this.  */
#undef HAVE_UNION_WAIT

#else /* not BSD_SYSTEM */

#include "usg5-2-2.h"

#define LIBS_SYSTEM -lmld
#define LIBS_DEBUG
#define START_FILES pre-crt0.o /usr/lib/crt1.o
#define LIB_STANDARD -lbsd -lc /usr/lib/crtn.o
/* #define LIBS_TERMCAP -lcurses */

#define C_SWITCH_SYSTEM -I/usr/include/bsd

/* Cancel certain parts of standard sysV support.  */
#undef NONSYSTEM_DIR_LIBRARY
#define SYSV_SYSTEM_DIR
#undef static

/* Don't try to use SIGIO or FIONREAD even though they are defined.  */
#define BROKEN_SIGIO
#define BROKEN_FIONREAD

/* Describe special kernel features.  */

#define HAVE_SYSVIPC

#if defined(emacs)
#include <bsd/sys/time.h>
#endif

/* The `select' in the system won't work for pipes,
   so don't use it.  */
#define BROKEN_SELECT

#define HAVE_DUP2
#define HAVE_GETWD
#define HAVE_GETTIMEOFDAY

#define HAVE_PTYS
#define HAVE_SOCKETS
/* #define BSTRING   Supposedly removed.  */

#undef NOMULTIPLEJOBS

#define CLASH_DETECTION

#if defined(HAVE_X_WINDOWS) && defined(HAVE_X11)
#define HAVE_VFORK		/* Graciously provided by libX.a */
#endif

#define utimes utime  /* Someone should check this.  */
/* ??? */
#define IRIS

#endif /* not BSD_SYSTEM */

/* High order bit must be stripped off nlist return values */
#define FIXUP_KERNEL_SYMBOL_ADDR(NL)  (NL)[0].n_value &= 0x7fffffff;

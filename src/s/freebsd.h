/* s/ file for freebsd system.  */

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif


#if 0 /* This much, alone, seemed sufficient as of 19.23.
	 But it seems better to be independent of netbsd.h.  */
#include "netbsd.h"

#undef LIB_GCC
#define LIB_GCC -lgcc
#undef NEED_ERRNO
#endif /* 0 */


/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* For mem-limits.h. */
#define BSD4_2

/* thses aren't needed, since we have getloadavg() */
#undef KERNEL_FILE
#undef LDAV_SYMBOL

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#if 0 /* These appear to be unnecessary for 1.1, and they break
	 emacs when compiled under FreeBSD-1.0.
	 Shawn M. Carey <smcarey@mailbox.syr.edu>  */
#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))
#endif

#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil
#define LIBS_TERMCAP -ltermcap
#define LIB_GCC -lgcc

/* Reread the time zone on startup. */
#define LOCALTIME_CACHE

#define SYSV_SYSTEM_DIR

/* freebsd has POSIX-style pgrp behavior. */
#undef BSD_PGRPS

#ifndef NO_SHARED_LIBS
/* These definitions should work for either dynamic or static linking,
   whichever is the default for `cc -nostdlib'. */
#define HAVE_TEXT_START		/* No need to define `start_of_text'. */
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define UNEXEC unexsunos4.o
#define RUN_TIME_REMAP

#ifndef N_TRELOFF
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)
#endif
#endif /* not NO_SHARED_LIBS */

#define HAVE_WAIT_HEADER
#define HAVE_GETLOADAVG
#define HAVE_TERMIOS
#define NO_TERMIO

/* freebsd uses OXTABS instead of the expected TAB3. */
#define TABDLY OXTABS
#define TAB3 OXTABS

/* this silences a few compilation warnings */
#undef BSD
#define BSD 199103

#define WAITTYPE int
/* get this since it won't be included if WAITTYPE is defined */
#ifdef emacs
#include <sys/wait.h>
#endif
#define WRETCODE(w) (_W_INT(w) >> 8)

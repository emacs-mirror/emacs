/* s- file for Interactive (ISC) Unix version 3.0 on the 386.  */

#include "isc2-2.h"

/* This has been moved into isc2-2.h.  */
/* #define HAVE_SOCKETS */

/* This appears on 3.0, presumably as part of what SunSoft call X2. */
#undef NO_X_DESTROY_DATABASE

/* People say that using -traditional causes lossage with `const',
   so we might as well try getting rid of -traditional.  */
#undef C_SWITCH_SYSTEM

/* We indirectly #include s/usg5-3.h, which says to use libX11_s and
   libc_s.  Martin Tomes <mt00@controls.eurotherm.co.uk> says that ISC
   has no libX11_s, and that linking with libc_s causes sbrk not to work.  */
#undef LIB_X11_LIB
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt -lnls -lnsl_s -lc

/* TIOCGWINSZ isn't broken; you just have to know where to find it.  */
#undef BROKEN_TIOCGWINSZ
#define NEED_SIOCTL

/* This does no harm, and is necessary for some ANSI compilers.  */
#define C_SWITCH_SYSTEM -D_SYSV3

/* This works around a bug in ISC 4.0 and 3.0; it fails
   to clear the "POSIX process" flag on an exec.
   It won't be needed for 4.1.  */
/* neg@brooktrout.com reported that he did not have this function
   on ISC 3.1.  I don't know who to believe or what to do,
   so I am leaving it alone until someone tells me
   precisely when this function is needed -- rms.  */
#define EXTRA_INITIALIZE __setostype (0)

/* s- file for Interactive (ISC) Unix version 3.0 on the 386.  */

#include "isc2-2.h"

/* These have been moved into isc2-2.h.  */
/* #define HAVE_SOCKETS
#define HAVE_SELECT */

/* This appears on 3.0, presumably as part of what SunSoft call X2. */
#undef NO_X_DESTROY_DATABASE

/* mt00@etherm.co.uk says this is needed for process.c.  */
#define HAVE_TIMEVAL

/* People say that using -traditional causes lossage with `const',
   so we might as well try getting rid of -traditional.  */
#undef C_SWITCH_SYSTEM

/* We indirectly #include s/usg5-3.h, which says to use libX11_s and
   libc_s.  Martin Tomes <mt00@controls.eurotherm.co.uk> says that ISC
   has no libX11_s, and that linking with libc_s causes sbrk not to work.  */
#undef LIB_X11_LIB
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt -lnls -lnsl_s -lc

/* marko@tekelec.com (Marko Rauhamaa) says that his linker couldn't
   find memmove, but that sounds crazy - I thought all SYSV
   descendants had that.  Let us know if this turns out to be wrong.  */
/* It is safe to have no parens around the args in the safe_bcopy call,
   and parens would screw up the prototype decl for memmove.  */
#define	memmove(d, s, n) safe_bcopy (s, d, n)

/* This is needed for ISC 4.0, but won't be needed for 4.1.  */

#include "isc3-0.h"

#define LIBS_SYSTEM -linet

#define ISC4_0
#define NEED_SIOCTL

/* fmcphers@csugrad.cs.vt.edu reported this was necessary.
   He used GCC.  I don't know what is needed with other compilers.  */
#ifdef __GNUC__
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt -lnls -lnsl_s -lcposix -lc
#endif

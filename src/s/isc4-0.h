#include "isc3-0.h"

#define LIBS_SYSTEM -linet

#define ISC4_0

/* fmcphers@csugrad.cs.vt.edu reported this was necessary.
   He used GCC.  I don't know what is needed with other compilers.  */
#ifdef __GNUC__
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt -lnls -lnsl_s -lcposix -lc
#endif

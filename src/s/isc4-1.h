#include "isc3-0.h"

/* ISC 4.1 has renamed __setostype, but also has fixed the bug
   for which we needed to call it; so just do nothing.  uddeborg@carmen.se.  */
#undef EXTRA_INITIALIZE

#define ISC4_1

#undef LIBS_SYSTEM
#define LIBS_SYSTEM -linet

/* uddeborg@carmen.se says we don't need -lPW or -lcposix.  */
#undef LIB_STANDARD

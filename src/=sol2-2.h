#include "s/sol2.h"

/* Take care of libucb as well as X Windows.  */
#undef LD_SWITCH_SYSTEM
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -R/usr/openwin/lib:/usr/ucblib
#else /* GCC */
#define LD_SWITCH_SYSTEM -Xlinker -R/usr/openwin/lib:/usr/ucblib
#endif /* GCC */

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#define LIB_STANDARD -lc -L/usr/ucblib -lucb
#endif

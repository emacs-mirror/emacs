#include "s-bsd4-2.h"

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#if __GNUC__ > 1
#define LD_SWITCH_SYSTEM -e __start -static
#else
#define LD_SWITCH_SYSTEM -e __start -Bstatic
#endif

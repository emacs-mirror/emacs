#include "hpux10.h"

#define HPUX11

#ifdef POSIX_SIGNALS
#undef POSIX_SIGNALS
#endif
#define POSIX_SIGNALS 1

/* SA_RESTART resets the timeout of `select', so don't use it.  */
#define BROKEN_SA_RESTART

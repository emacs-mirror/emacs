#include "hpux10.h"

#define HPUX11

#ifdef POSIX_SIGNALS
#undef POSIX_SIGNALS
#endif
#define POSIX_SIGNALS 1

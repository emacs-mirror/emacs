/* This is needed for ISC 4.0, but won't be needed for 4.1.  */

#include "isc3-0.h"

/* This works around a bug in ISC 4.0; it fails
   to clear the "POSIX process" flag on an exec.  */
#define EXTRA_INITIALIZE __setostype (0)


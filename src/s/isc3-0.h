/* s- file for Interactive (ISC) Unix version 3.0 on the 386.  */

#include "s/isc2-2.h"

/* These have been moved into s-isc2-2.h.  */
/* #define HAVE_SOCKETS
#define HAVE_SELECT */

/* This appears on 3.0, presumably as part of what SunSoft call X2. */
#undef NO_X_DESTROY_DATABASE

/* mt00@etherm.co.uk says this is needed for process.c.  */
#define HAVE_TIMEVAL

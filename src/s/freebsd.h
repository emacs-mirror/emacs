/* s/ file for freebsd system.  */

/* '__FreeBSD__' is defined by the preprocessor on FreeBSD-1.1 and up.
   Earlier versions do not have shared libraries, so inhibit them.  */
#ifndef __FreeBSD__
#define NO_SHARED_LIBS
#endif

#include "netbsd.h"

#undef LIB_GCC
#define LIB_GCC -lgcc
#undef NEED_ERRNO

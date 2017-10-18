/* s- file for building Emacs on AIX 3.2.
   This is a guess, since AIX 3.2 hasn't been released yet.  */

#include "s-aix3-1.h"

/* Stop m-ibmrs6000.h from defining this.  */
#define SPECIFY_X11R4 1

/* These work in 3.2, and are optimized when string.h is used.  */
#undef index
#undef rindex

/* s- file for building Emacs on AIX 3.2.  */

#include "aix3-1.h"

/* No need to define this--the header files indicate X11R4,
   and that's supposedly what 3.2 will come with.  */
#undef SPECIFY_X11R4

#define C_SWITCH_SYSTEM -ma
#define HAVE_ALLOCA
#undef rindex
#undef index

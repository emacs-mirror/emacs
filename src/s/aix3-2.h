/* s- file for building Emacs on AIX 3.2.  */

#include "aix3-1.h"

#define AIX3_2

/* No need to define this--the header files indicate X11R4,
   and that's supposedly what 3.2 will come with.  */
#undef SPECIFY_X11R4

#ifndef __GNUC__
#define C_SWITCH_SYSTEM -ma
#endif
#define HAVE_ALLOCA
#undef rindex
#undef index

#define HAVE_FSYNC

/* With this defined, a gcc-compiled Emacs crashed in realloc under AIX
   3.2, and a cc-compiled Emacs works with this undefined.
   --karl@cs.umb.edu.  */
#undef SYSTEM_MALLOC

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
#ifndef __GNUC__
#define C_SWITCH_DEBUG -g
#define C_SWITCH_OPTIMIZE
#endif

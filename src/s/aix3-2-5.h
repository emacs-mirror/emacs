/* s- file for building Emacs on AIX 3.2.5.  */

#include "aix3-2.h"

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
#ifndef __GNUC__
#undef C_SWITCH_DEBUG
#undef C_SWITCH_OPTIMIZE
#define C_SWITCH_DEBUG
#define C_SWITCH_OPTIMIZE -O
#endif

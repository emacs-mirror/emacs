#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd

#ifdef __alpha
#define LD_SWITCH_SYSTEM
#else
#define LD_SWITCH_SYSTEM	-non_shared
#endif

#define SYSV_SYSTEM_DIR

/* Declare malloc and realloc in a way that is clean.
   But not in makefiles!  */

#ifndef THIS_IS_YMAKEFILE
extern void *malloc (), *realloc ();
#endif

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* Define _BSD to tell the include files we're running under
   the BSD universe and not the SYSV universe.  */

#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd

#define read sys_read
#define write sys_write
#define open sys_open
#define close sys_close

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

#define SYSV_SYSTEM_DIR

/* Declare malloc and realloc in a way that is clean.
   But not in makefiles!  */

#ifndef NOT_C_CODE
#ifndef THIS_IS_YMAKEFILE
/* We need these because pointers are larger than the default ints.  */
#include <alloca.h>
#include <string.h>
extern void *malloc (), *realloc ();
extern long *xmalloc (), *xrealloc ();
#endif
#endif

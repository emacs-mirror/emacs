/* s/ file for 386bsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef LIB_STANDARD
#define LIB_STANDARD -lc $(GNULIB_VAR)

/* The following should be set to /netbsd if you are running netbsd > 0.8
   Or just link /netbsd -> /386bsd  */
#undef KERNEL_FILE
#define KERNEL_FILE "/386bsd"

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* Need to use GNU make, as system make has problems */
#define MAKE_COMMAND gmake
#define LIBS_DEBUG

/* For mem-limits.h.  */
#define BSD4_2

/* This affects a declaration in xrdb.c.  */
#define DECLARE_GETPWUID_WITH_UID_T

/* arch-tag: 8a114892-0863-4285-a4cb-3d66ba2d8e7b
   (do not change this comment) */

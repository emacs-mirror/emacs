/* machine description file for Alliant Concentrix 4.0 or later.
   Use alliant.h for versions 2 and 3.  */

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-2"  */

#include "alliant.h"

/* Concentrix uses a different kernel symbol for load average. */

#undef  LDAV_SYMBOL		/* Undo definition in s-bsd4-2.h */
#define LDAV_SYMBOL "_Loadavg"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (x * 100 / LOADAVG_SCALE)

/* include <sys/param.h> for the definition of LOADAVG_SCALE, and also
   LOADAVG_SIZE, the number of items in the Loadavg array. */

/* arch-tag: cf917b55-c95e-4079-a4d1-d31e00c61b66
   (do not change this comment) */

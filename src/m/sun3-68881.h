/* sun3-68881.h, for a Sun 3, using the 68881.  */

#include "sun3.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Sun with 68881 co-processor (-machine=sun3-68881;
			     -opsystem=bsd4-2 or -opsystem=sunos4)

  Versions 1, 2, and 3 of the operating system are derived from
  Berkeley 4.2, meaning that you should use -opsystem=bsd4-2.

  For SunOS release 4 on a Sun 3 with a 68881, use -machine=sun3-68881
  and -opsystem=sunos4.  See the file share-lib/SUNBUG for how to
  solve problems caused by bugs in the "export" version of SunOS 4.
NOTE-END  */

/* In case we are using floating point, work together with crt0.c.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE -f68881
#endif

#define sun_68881
#define START_FILES crt0.o /usr/lib/Mcrt1.o

/* arch-tag: f8659e89-5f5e-4921-940c-814a5786b901
   (do not change this comment) */

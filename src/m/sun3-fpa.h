/* sun3-fpa.h, for a Sun 3, using the Sun fpa.  */

#include "sun3.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Sun with FPA co-processor (-machine=sun3-fpa;
			   -opsystem=bsd4-2 or -opsystem=sunos4)

  Versions 1, 2, and 3 of the operating system are derived from
  Berkeley 4.2, meaning that you should use -opsystem=bsd4-2.

  For SunOS release 4 on a Sun 3 with an FPA, use -machine=sun3-fpa
  and -opsystem=sunos4.  See the file share-lib/SUNBUG for how to
  solve problems caused by bugs in the "export" version of SunOS 4.
NOTE-END  */

/* In case we are using floating point, work together with crt0.c.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE -ffpa
#endif

#define sun_fpa
#define START_FILES crt0.o /usr/lib/Wcrt1.o

/* arch-tag: db287fbb-966f-4a70-a3f4-a6768c09326b
   (do not change this comment) */

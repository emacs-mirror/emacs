/* sun3-soft.h, for a Sun 3, using the Sun with software floating point.  */

#include "sun3.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Sun with software floating point (-machine=sun3-soft;
			          -opsystem=bsd4-2 or -opsystem=sunos4)

  Versions 1, 2, and 3 of the operating system are derived from
  Berkeley 4.2, meaning that you should use -opsystem=bsd4-2.

  If you want to use software floating point on SunOS release 4 on a
  Sun 3, use -machine=sun3-68881 and -opsystem=sunos4.  See the file
  share-lib/SUNBUG for how to solve problems caused by bugs in the
  "export" version of SunOS 4.
NOTE-END  */

/* In case we are using floating point, work together with crt0.c.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE -fsoft
#endif

#define sun_soft
#define START_FILES crt0.o /usr/lib/Fcrt1.o

/* arch-tag: 76e9d7e6-66a8-4c4f-b0a5-335d082e5720
   (do not change this comment) */

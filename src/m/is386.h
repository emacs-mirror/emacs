/* machine description file for Integrated Solutions 386 machine.  */

#include "intel386.h"

/* The following line tells the configuration script what sort of
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Intel 386 (-machine=intel386 or -machine=is386.h)

  The possibilities for -opsystem are: bsd4-2, usg5-2-2, usg5-3,
  isc2-2, 386-ix, esix, or xenix.

  18.58 should support a wide variety of operating systems.
  Use isc2-2 for Interactive 386/ix version 2.2.
  Use 386ix for prior versions.
  Use esix for Esix.  It isn't clear what to do on an SCO system.

  -machine=is386 is used for an Integrated Solutions 386 machine.
  It may also be correct for Microport systems.
NOTE-END  */

#define LIBX10_MACHINE -lnsl_s
#define LIBX11_MACHINE -lnsl_s

#define LIBS_DEBUG -lg

/* arch-tag: b6b7e6ec-8b6c-440b-b9c8-961e4bebf0cf
   (do not change this comment) */

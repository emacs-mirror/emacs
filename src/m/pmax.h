#include "mips.h"

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-2"  */

#undef BIG_ENDIAN
#define LIBS_DEBUG

/* This line starts being needed with ultrix 4.0.  */
/* You may need to delete it on version 3.1.  */
#define START_FILES pre-crt0.o /usr/lib/cmplrs/cc/crt0.o

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

/* In Ultrix 4.1, XvmsAlloc.o in libX11.a seems to insist
   on defining malloc itself.  This should avoid conflicting with it.  */
#define SYSTEM_MALLOC

/* Machine description file for DEC MIPS machines.  */

#include "mips.h"

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"  

NOTE-START
Use -opsystem=osf1 for OSF/1, and -opsystem=bsd4-3 otherwise.
NOTE-END  */

#undef BIG_ENDIAN
#undef LIB_STANDARD
#undef START_FILES
#undef COFF
#undef TERMINFO
#define MAIL_USE_FLOCK
#define HAVE_UNION_WAIT

/* This line starts being needed with ultrix 4.0.  */
/* You must delete it for version 3.1.  */
#define START_FILES pre-crt0.o /usr/lib/cmplrs/cc/crt0.o

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

#if 0
/* I don't see any such conflict in Ultrix 4.2, 4.2a, or 4.3.  And
   the relocating allocator is a real win.  -JimB  */

/* In Ultrix 4.1, XvmsAlloc.o in libX11.a seems to insist
   on defining malloc itself.  This should avoid conflicting with it.  */
#define SYSTEM_MALLOC
#endif

/* Override what m-mips.h says about this.  */
#undef LINKER

/* Ultrix 4.2 (perhaps also 4.1) implements O_NONBLOCK
   but it doesn't work right;
   and it causes hanging in read_process_output.  */
#define BROKEN_O_NONBLOCK

#ifdef OSF1
#undef C_ALLOCA
#define HAVE_ALLOCA
#endif

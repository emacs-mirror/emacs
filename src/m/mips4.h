/* machine description file for Mips running RISCOS version 4.  */

#include "mips.h"

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
Use -opsystem=usg5-2-2 normally, or -opsystem=bsd4-3 with the BSD
world.
NOTE-END  */

#if 0
/* Define MIPS2 if you have an R6000 or R4000.  */
#define MIPS2
#endif

#ifdef __GNUC__
#define C_DEBUG_SWITCH -g -O
#else
#ifdef MIPS2
#define C_DEBUG_SWITCH -DMips -systype bsd43 -g3 -O -Olimit 2000 -mips2
#else
#define C_DEBUG_SWITCH -DMips -systype bsd43 -g3 -O -Olimit 2000
#endif
#endif

#ifdef TERMINFO
#undef TERMINFO
#endif

#define START_FILES pre-crt0.o /lib/crt1.o
/* Used to have -lisode, but jlp@math.byu.edu says remove it
   (for RISCOS 4.52).  */
#define LIB_STANDARD -lmld -lc /lib/crtn.o


#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -systype bsd43 -g3 -D 800000

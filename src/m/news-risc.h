/* news-risc.h is for the "RISC News".  */

#include "mips.h"
#undef LIBS_MACHINE

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

#define LIBS_MACHINE -lmld

#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -x -D 800000

/* #define C_OPTIMIZE_SWITCH -O2 */
#define C_OPTIMIZE_SWITCH -O

#define C_DEBUG_SWITCH -g3

#undef TERMINFO

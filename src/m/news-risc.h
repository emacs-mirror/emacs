/* news-risc.h is for the "RISC News".  */

#include "mips.h"

#ifdef NEWSOS5

/* NEWS-OS 5.0.2 */

#define LIBS_MACHINE -lmld

#ifdef __GNUC__
#define LD_SWITCH_MACHINE -g -Xlinker -D -Xlinker 800000
#else
#define C_DEBUG_SWITCH -g3
#define C_OPTIMIZE_SWITCH -g3
#define LD_SWITCH_MACHINE -g3 -D 800000 -non_shared
#endif

#else /* not NEWSOS5 */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="bsd4-3"  */

#define COFF
#undef LD_SWITCH_MACHINE
#ifdef __GNUC__
#define LD_SWITCH_MACHINE -Xlinker -x -Xlinker -D -Xlinker 800000
#else
#define LD_SWITCH_MACHINE -x -D 800000
#endif

/* #define C_OPTIMIZE_SWITCH -O2 */
#define C_OPTIMIZE_SWITCH -O

#ifndef __GNUC__
#define C_DEBUG_SWITCH -g3
#endif

#undef TERMINFO

/* We have no mode_t.  */
#define NO_MODE_T

/* Don't use the definitions in m/mips.h.  */
#undef LINKER
#define LINKER $(CC) -nostdlib
#undef LIBS_MACHINE
#define LIBS_MACHINE -lmld

#undef KERNEL_FILE
#define KERNEL_FILE "/vmunix"

/* System's malloc, realloc, calloc and so on have bad prototypes,
   using char * instead of void *, so tell gmalloc not to use the
   prototypes.  */
#define BROKEN_PROTOTYPES

#endif /* not NEWSOS5 */

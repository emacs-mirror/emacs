#include "mips.h"


/* Data type of load average, as read out of kmem.  */

#undef LOAD_AVE_TYPE
#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#undef LOAD_AVE_CVT
#define LOAD_AVE_CVT(x) ((int) ((x) * 100.0))

/* Don't use the ordinary -g for debugging in cc */

#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g

/* w.prediger@m30x.nbg.scn.de seems to say this is needed.
   But let's try without it.  */
/* #define C_ALLOCA */

/* This system uses a slightly nonstandard variant of elf format.  */
#undef UNEXEC
#define UNEXEC unexelf.o
#define ELF_BSS_SECTION_NAME ".sbss"

#define BSTRING

#define LIB_STANDARD /usr/ccs/lib/libc.a

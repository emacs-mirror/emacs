/* m- file for NEC EWS4800 RISC series. */
#undef LIB_STANDARD
#undef C_DEBUG_SWITCH
#define INHIBIT_BSD_TIME
#undef USG
#include "mips.h"
#define USG
#undef UNEXEC
#define UNEXEC unexelf.o
#undef LIBS_MACHINE
#undef LD_SWITCH_MACHINE
#undef START_FILES
#undef DATA_START
#undef LIB_STANDARD
#undef C_SWITCH_MACHINE
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -O  -KOlimit=3000 -ZXNd=5000
#endif  /* !__GNUC__ */

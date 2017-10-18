/* m-news-risc.h is for the "RISC News".  */

/* Override s-usg5-4.h.  */
#ifdef USG
#undef UNEXEC
#undef LIBX11_SYSTEM
#undef LOAD_AVE_CVT
#define INHIBIT_BSD_TIME
#undef START_FILES
#undef LIB_STANDARD
#endif

#define COFF

#include "m-mips.h"

#undef LIBS_MACHINE
#define LIBS_MACHINE -lmld

#define C_OPTIMIZE_SWITCH -O

#undef LD_SWITCH_MACHINE

#ifndef USG
#undef TERMINFO

#undef LINKER /* Override m-mips.h.  */

#define LD_SWITCH_MACHINE -x -D 800000

#undef  C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g3
#endif

#ifdef USG
#undef LIB_STANDARD
#define LIB_STANDARD GNULIB -lsocket -lnsl -lc /usr/ccs/lib/crtn.o

#undef LIBS_TERMCAP
#define LIBS_TERMCAP -ltermlib

#undef C_SWITCH_SYSTEM

#undef  C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g

#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -D 10000000 -g3

#undef USG_SHARED_LIBRARIES

#undef START_FILES
#define START_FILES pre-crt0.o /usr/ccs/lib/crt1.o /usr/ccs/lib/values-Xt.o

#undef  TEXT_START
#define TEXT_START 0x400000
#undef  DATA_START
#define DATA_START 0x10000000

#undef DATA_SEG_BITS
#define DATA_SEG_BITS 0x10000000

#undef C_ALLOCA
#define HAVE_ALLOCA
#if defined(__GNUC__) && !defined(alloca)
#define alloca(n) __builtin_alloca(n)
#endif

/* replace bsd getwd() with SysV getcwd() */
#undef HAVE_GETWD
#undef HAVE_GETTIMEOFDAY

#define LOAD_AVE_TYPE long
#endif

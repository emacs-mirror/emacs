#include "irix5-0.h"

/* Irix 6 tries to do 64 bits, but doesn't do it fully,
   so inhibit that.  */
#define IRIX_FORCE_32_BITS

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -32
#endif

/* This macro definition, which we inherited from irix5-0.h,
   is needed in configure on Irix 5, but gets in the way there
   on Irix 6.  So get rid of it except in Makefile.in where we need it.  */
#ifndef THIS_IS_MAKEFILE
#undef C_SWITCH_SYSTEM
#endif

/* The only supported configuration of GCC under IRIX6.x produces
   n32 MIPS ABI binaries and also supports -g. */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g
#endif

#undef SA_RESTART

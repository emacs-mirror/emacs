#define IRIX6
#define IRIX6_5
#include "irix5-0.h"

/* Irix 6 tries to do 64 bits, but doesn't do it fully,
   so inhibit that.  */
#define IRIX_FORCE_32_BITS

#ifndef __GNUC__
#ifndef IRIX6_5
#define LD_SWITCH_SYSTEM -32
#else
#define LD_SWITCH_SYSTEM -n32
#endif
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
#else
/* Optimize, inaccurate debugging, increase limit on size of what's
   optimized.

   This should also be applicable other than on Irix 6.5, but I don't
   know for which compiler versions.  -- fx */
#define C_DEBUG_SWITCH -g3 -O -OPT:Olimit=3500
#endif

#undef SA_RESTART

/* It turns out that the #define in irix5-0.h is needed in Irix 6 as well.  */
#if 0
/* Canced the #define that is in irix5-0.h.  */
#undef ospeed
#endif

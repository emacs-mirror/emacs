#define IRIX6
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

/* It turns out that the #define in irix5-0.h is needed in Irix 6 as well.  */
#if 0
/* Canced the #define that is in irix5-0.h.  */
#undef ospeed
#endif

/* Cancel some #define's in usg5-4.h.
   Larry Hunter <hunter@nlm.nih.gov> said this was needed
   for Irix 6.5.  Let's see if it is safe in 6.N, N<5, as well.  */
#undef TIOCSIGSEND
/* Extrapolating from Irix 6.5, the problem is that (at least) the
   bzero definition breaks what the X headers do.  The following means
   that we lack prototypes for these functions, and we presumably lose
   at least in the 64-bit ABI (though that's only supported on Irix
   6.5, which I can test).  We may be saved by the fact that these
   appear to be intrinsics in the SGI (Cray) compiler.  It's probably
   appropriate to include strings.h here, but I can't test it.  See
   irix6-5.h.  -- fx  */
#undef bcopy
#undef bcmp
#undef bzero

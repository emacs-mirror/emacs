#include "hpux9shr.h"

#define HPUX10

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef srand48
#undef HAVE_RANDOM
#define HPUX10
#define FORCE_ALLOCA_H

#ifdef LIBS_SYSTEM
#undef LIBS_SYSTEM
#endif
#ifdef HPUX_NET
#define LIBS_SYSTEM -ln -l:libdld.sl
#else
#define LIBS_SYSTEM -l:libdld.sl
#endif

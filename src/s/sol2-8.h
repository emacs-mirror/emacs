/* Handle Solaris 2.8.  */

#include "sol2-5.h"

/* Redundant but differing definitions for bcopy, bcmp, and bzero are
   causing problems.  Get rid of the emacs overrides for these. */

#ifdef bcopy
#undef bcopy
#endif
#ifdef bcmp
#undef bcmp
#endif
#ifdef bzero
#undef bzero
#endif

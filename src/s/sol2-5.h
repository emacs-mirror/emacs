/* Handle Solaris 2.5.  */

#include "sol2-4.h"

/* Don't use the shared libraries for -lXt and -lXaw,
   to work around a linker bug in Solaris 2.5.
   (This also affects the other libraries used specifically for
   the X toolkit, which may not be necessary.)  */
#define LIBXT_STATIC

#ifdef __GNUC__
#define STATIC_OPTION -static
#else
#define STATIC_OPTION -Bstatic
#endif

#include "intel386.h"

#ifndef __GNUC__
/* Allow emacs to link with "bcopy()" unresolved.  Works around a
   problem where /usr/lib/libX11.so provides bcopy, but
   /usr/ccs/lib/libX11.so does not.  */
#define LD_SWITCH_X_DEFAULT -Wl,-z,nodefs
#else /* __GNUC__ */

/* Assuming we are using GNU ld, pass a -R option to it
   so that shared libraries will be found at execution time
   just as they are found at link time.  */
#define LD_SWITCH_X_DEFAULT -Xlinker LD_SWITCH_X_SITE_AUX

#endif /* __GNUC__ */

/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */

#undef LD_SWITCH_SYSTEM
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX -R /usr/dt/lib -L /usr/dt/lib
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX` -R /usr/dt/lib -L /usr/dt/lib
#endif /* GCC */

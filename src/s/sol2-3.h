#include "sol2.h"

/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO

/* Override LD_SWITCH_SYSTEM: add  -L /usr/ccs/lib to the sol2.h value.  */

#undef LD_SWITCH_SYSTEM

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */

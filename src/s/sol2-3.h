#include "sol2.h"

/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO

/* Override LD_SWITCH_SYSTEM: add  -L /usr/ccs/lib to the sol2.h value.  */

#undef LD_SWITCH_SYSTEM

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX
#else /* GCC */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib -Xlinker LD_SWITCH_X_SITE_AUX
#endif /* GCC */

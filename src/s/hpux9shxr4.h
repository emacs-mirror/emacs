#include "hpux9shr.h"

#undef  C_SWITCH_X_SYSTEM
#define C_SWITCH_X_SYSTEM -I/usr/include/Motif1.1

#undef  LD_SWITCH_X_DEFAULT
#define LD_SWITCH_X_DEFAULT -L/usr/lib/Motif1.1


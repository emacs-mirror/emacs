#define ORDINARY_LINK
#define HPUX_USE_SHLIBS
#define RUN_TIME_REMAP

#include "hpux9.h"

#if 0 /* No longer needed, since in current GCC -g no longer does that.  */
/* We must turn off -g since it forces -static.  */
#ifdef __GNUC__
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH
#endif
#endif

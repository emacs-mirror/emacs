/* System description file for hpux version 9.  */   

#include "hpux8.h"

#define HPUX9

/* Try some debugging and see if gnu malloc hurts us */
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC

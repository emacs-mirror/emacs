#include "sunos4-1.h"

/* jik@gza.com says this works now.  */
/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC

/* barrie@calvin.demon.co.uk says memmove is missing.  */
#ifndef SYSTEM_MALLOC
#define MEMMOVE_MISSING
#endif

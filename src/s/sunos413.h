#include "sunos4-1.h"

/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC

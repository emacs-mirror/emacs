#include "sunos4-1.h"

/* jik@gza.com says this works now.  */
/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC

/* barrie@calvin.demon.co.uk says memmove is missing.  */
#ifndef SYSTEM_MALLOC
#define MEMMOVE_MISSING
#endif

#define USE_MMAP_FOR_BUFFERS 1

/* arch-tag: ebd184b0-9084-4306-8e71-c0437330e1e1
   (do not change this comment) */

#include "ptx4.h"

/* Gelling Kevan (gellingk.eurostar@ems.rail.co.uk)
   says that PTX 4.2.3 needs this version.  */
#undef SETUP_SLAVE_PTY
#define SETUP_SLAVE_PTY \
    if (ioctl (xforkin, I_PUSH, "ptem") == -1)    \
      fatal ("ioctl I_PUSH ptem", errno);        \
    if (ioctl (xforkin, I_PUSH, "ldterm") == -1)  \
      fatal ("ioctl I_PUSH ldterm", errno);

/* Gelling Kevan (gellingk.eurostar@ems.rail.co.uk)
   says that gmalloc.c needs _POSIX_SOURCE.
   This defines _POSIX_SOURCE only for gmalloc.c.  */
#ifdef _MALLOC_INTERNAL
#define _POSIX_SOURCE
#endif

/* arch-tag: 10a9fab3-9e84-4e9e-9535-6ff42baf9e77
   (do not change this comment) */

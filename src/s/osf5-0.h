#define OSF5
#include "osf1.h"

/* It's enough to define _OSF_SOURCE instead of _BSD.  */

#undef C_SWITCH_SYSTEM
#define C_SWITCH_SYSTEM	-D_OSF_SOURCE

#ifndef NSIG			/* _OSF_SOURCE seems to get us this */
#define NSIG __sys_nsig
#endif

/* We have missing/inconsistent prototypes on 5.0, at least.  */
#define INHIBIT_X11R6_XIM

#define USE_MMAP_FOR_BUFFERS	1

#define TERMINFO
#define LIBS_TERMCAP -lcurses

#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: 89580064-dd8c-4533-a47c-0f92d8090945
   (do not change this comment) */

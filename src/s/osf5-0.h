#define OSF5
#include "osf1.h"

/* The -BSD loses when _XOPEN_SOURCE gets defined by configure in
   OSF 5.  It's possible this will need to be reverted for earlier
   versions (for which OSF5 isn't defined).  */
#undef C_SWITCH_SYSTEM
#define C_SWITCH_SYSTEM	-D_OSF_SOURCE
#define WAIT_USE_INT
#define SYS_SIGLIST_DECLARED
#define sys_siglist __sys_siglist
#define NSIG __sys_nsig

/* We have missing/inconsistent prototypes on 5.0, at least.  */
#define INHIBIT_X11R6_XIM

#include "bsd4-3.h"

/* Identify OSF1 for the m- files. */

#define OSF1

/* The following used to be done, but -BSD loses when _XOPEN_SOURCE
   gets defined by configure, at least on OSF5.  It's possible this
   will need to be reverted for earlier versions (for which OSF5 isn't
   defined).  */
#if 0
#define C_SWITCH_SYSTEM	-D_BSD
#define LIBS_SYSTEM	-lbsd

#else

#define C_SWITCH_SYSTEM	-D_OSF_SOURCE
#define WAIT_USE_INT
#define SYS_SIGLIST_DECLARED
#define sys_siglist __sys_siglist
#define NSIG __sys_nsig
#endif /* 0 */

#define GETPGRP_NO_ARG

#define SYSV_SYSTEM_DIR

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  We convert it to a -rpath option
   which is what OSF1 uses.  */
#define LD_SWITCH_SYSTEM `echo LD_SWITCH_X_SITE_AUX | sed -e 's/-R/-Wl,-rpath,/'`

#define HAVE_TERMIOS

#ifndef __GNUC__
/* Optimize, inaccurate debugging.  */
#define C_DEBUG_SWITCH -g3
#endif

#ifndef OSF5			/* fixed in 5.0 */
/* Hack alert!  For reasons unknown to mankind the string.h file insists
   on defining bcopy etc. as taking char pointers as arguments.  With
   Emacs this produces an endless amount of warning which are harmless,
   but tends to flood the real errors.  This hack works around this problem
   by not prototyping.  */
#define bcopy string_h_bcopy
#define bzero string_h_bzero
#define bcmp  string_h_bcmp
#include <string.h>
#undef bcopy
#undef bzero
#undef bcmp
#endif

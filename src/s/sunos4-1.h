#include "sunos4-0.h"

/* 4.1.1 makes these system calls interruptible.  */

#define read sys_read
#define write sys_write
#define open sys_open
#define close sys_close

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_CLOSE
#define INTERRUPTIBLE_IO

/* Cause the compilation of oldxmenu to use the right -I option.  */
#define OLDXMENU_OPTIONS CFLAGS=C_SWITCH_SYSTEM

/* Some systems do not run the Network Information Service, but have
   modified the shared C library to include resolver support without
   also changing the C archive library (/usr/lib/libc.a).  If we detect
   the presence of res_init, use -lresolv to supplement libc.a.  */
#ifdef HAVE_RES_INIT
#define LIBS_SYSTEM -lresolv
#endif

/* Tell GNU malloc to compensate for a bug in localtime.  */
#define SUNOS_LOCALTIME_BUG

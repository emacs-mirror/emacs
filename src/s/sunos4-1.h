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

/* Ethan Bradford <ethanb@kepler.astro.washington.edu> says this allows
   open-network-stream to find remote hosts.  */
#define LIBS_SYSTEM -lresolv

/* system description file for hpux version 8.
   This contains changes that were suggested "for the hp700".
   They were not needed for the 800.
   Our conjecture that they are needed for hpux version 8,
   which is what runs on the 700.  */

#include "hpux.h"

#define LIB_X11_LIB -L/usr/lib/X11R4 -lX11
#define C_SWITCH_SYSTEM -I/usr/include/X11R4

/* Don't use shared libraries.  unexec doesn't handle them.  */
#define LD_SWITCH_SYSTEM -a archive

/* system description file for hpux version 8.
   This contains changes that were suggested "for the hp700".
   They were not needed for the 800.
   Our conjecture that they are needed for hpux version 8,
   which is what runs on the 700.  */

#include "hpux.h"

#define HPUX8

#define LIB_X11_LIB -L/usr/lib/X11R4 -lX11
#define C_SWITCH_SYSTEM -I/usr/include/X11R4

/* Don't use shared libraries.  unexec doesn't handle them.
   Note GCC automatically passes -a archive to ld, and it has its own
   conflicting -a.  */
#ifdef __GNUC__
#define LD_SWITCH_SYSTEM  -L/usr/lib/X11R4 -Xlinker -a -Xlinker archive

/* No need to specify roundabout way of linking temacs.  */
#define ORDINARY_LINK
#else
#define LD_SWITCH_SYSTEM -a archive  -L/usr/lib/X11R4
#endif

/* Specify compiler options for compiling oldXMenu.  */
#define OLDXMENU_OPTIONS CFLAGS=-I/usr/include/X11R4

/* Some hpux 8 machines seem to have TIOCGWINSZ,
   and none have sioctl.h, so might as well define this.  */
#define NO_SIOCTL_H

#define HAVE_RANDOM

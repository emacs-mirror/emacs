/* System description file for hpux version 9.  */

#include "hpux8.h"

#define HPUX9

/* If Emacs doesn't seem to work when built to use GNU malloc, you
   probably need to get the latest patches to the HP/UX compiler.
   See `etc/MACHINES' for more information.  */
#if 0
#define SYSTEM_MALLOC 1
#undef GNU_MALLOC
#undef REL_ALLOC
#endif

#ifndef __GNUC__
/* Make room for enough symbols, so dispnew.c does not fail.  */
#define C_SWITCH_SYSTEM -Wp,-H200000 -D_BSD
#else
#define C_SWITCH_SYSTEM -D_BSD
#endif

#if 0 /* These definitions run into a bug in hpux
	 whereby trying to disable the vdsusp character has no effect.
	 supposedly there is no particular need for this.  */
/* neal@ctd.comsat.com */
#undef HAVE_TERMIO
#define HAVE_TERMIOS
#define NO_TERMIO
#endif

/* According to ngorelic@speclab.cr.usgs.gov,
   references to the X11R4 directoriess in these variables
   (inherited from hpux8.h)
   cause the wrong libraries to be found,
   and the options to specify the X11R5 directories are unnecessary
   since the R5 files are found without them.  */
#undef LIB_X11_LIB
#undef C_SWITCH_X_SYSTEM
#undef LD_SWITCH_X_DEFAULT
/* However, HPUX 9 has Motif includes in a strange place.
   So search that place.  These definitions assume that X11R5 is being
   used -- if X11R4 is used, "s/hpux9-x11r4.h" gets loaded instead.  */
#define C_SWITCH_X_SYSTEM -I/usr/include/Motif1.2
#define LD_SWITCH_X_DEFAULT -L/usr/lib/Motif1.2

/* HP-UX doesn't supply nor need Xmu.  */
#define LIBXMU

/* zoo@armadillo.com says we don't need -lXext in HPUX 9.  */
#undef LIBX11_SYSTEM

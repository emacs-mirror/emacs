/* system description file for hpux version 8.
   This contains changes that were suggested "for the hp700".
   They were not needed for the 800.
   Our conjecture that they are needed for hpux version 8,
   which is what runs on the 700.  */

#include "hpux.h"

#define HPUX8

/* dob@inel.gov says HPUX 8.07 needs this.  He was using X11R5, I think.  */
#define LIBX11_SYSTEM -lXext

#define LIB_X11_LIB -L/usr/lib/X11R5 -L/usr/lib/X11R4 -lX11
#define C_SWITCH_X_SYSTEM -I/usr/include/X11R5 -I/usr/include/X11R4

/* Don't use shared libraries.  unexec doesn't handle them.
   Note GCC automatically passes -a archive to ld, and it has its own
   conflicting -a.  */
#ifdef __GNUC__
#define LD_SWITCH_SYSTEM -Xlinker -a -Xlinker archive

/* No need to specify roundabout way of linking temacs.  */
#define ORDINARY_LINK
#else
#if defined(hp9000s700) || defined(__hp9000s700)
#define LD_SWITCH_SYSTEM -a archive -L/lib/pa1.1
#else
#define LD_SWITCH_SYSTEM -a archive
#endif
#endif

#if 0 /* This should no longer be necessary now that
	 C_SWITCH_... are passed down when compiling oldXMenu.  */
/* Specify compiler options for compiling oldXMenu.  */
#define OLDXMENU_OPTIONS CFLAGS="-I/usr/include/X11R5 -I/usr/include/X11R4"
#endif

/* Some hpux 8 machines seem to have TIOCGWINSZ,
   and none have sioctl.h, so might as well define this.  */
#define NO_SIOCTL_H

#if 0 /* autoconf should be detecting the presence or absence of 
	 random and srandom now.  */
/* If you use X11R4 you must define this.  If you use
   X11R5 you must comment this out */
/* #define HAVE_RANDOM */
#define random foo_random
#define srandom foo_srandom
#endif

#if 0  /* This seems to be spurious.  */
/* "X11R5" on hpux8 doesn't have this function, which is supposed to exist
   in X11R5.  Maybe things will work if we just don't call it.  */
#define NO_XRM_SET_DATABASE
#endif

#include "usg5-4.h"

#define SOLARIS2

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

/* Here is how to find X Windows.  LD_SWITCH_X_SITE_AUX gives an -R option
   says where to find X windows at run time.  */
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM LD_SWITCH_X_SITE_AUX
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX`
#endif /* GCC */

#define HAVE_VFORK

/* Gregory Neil Shapiro <gshapiro@hhmi.org> reports the Motif header files
   are in this directory on Solaris 2.4.  Let's guess that's true
   for some earlier versions too.  */
#define C_SWITCH_X_SYSTEM -I/usr/dt/include

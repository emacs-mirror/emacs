/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */

#undef LD_SWITCH_SYSTEM

/* `#ifdef USE_MOTIF' won't work here, since USE_MOTIF isn't defined yet.
   Instead, dynamically check whether USE_MOTIF expands to something.  */
#define NOT_USING_MOTIF { set x USE_MOTIF; test "$$2" = "USE_MOTIF"; }

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM_TEMACS -L/usr/ccs/lib LD_SWITCH_X_SITE_AUX \
  `NOT_USING_MOTIF || echo ' -R/usr/dt/lib'`
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in temacs.  */
#define LD_SWITCH_SYSTEM_TEMACS -L/usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX` \
  `NOT_USING_MOTIF || echo ' -R/usr/dt/lib -L/usr/dt/lib'`
#endif /* GCC */

/* Gregory Neil Shapiro <gshapiro@hhmi.org> reports the Motif header files
   are in this directory on Solaris 2.4.  */
#define C_SWITCH_X_SYSTEM -I/usr/dt/include

/* arch-tag: 6f0de37b-cfda-427a-a5ae-b83ed54aaae7
   (do not change this comment) */

/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

/* Solaris 2.4 has a broken vfork.  So we don't use it;
   we use the alternate definition in sysdep.c.
   But a header file has a declaration 
   that conflicts with the definition of vfork in sysdep.c.
   This kludge should prevent the conflict.  */
#define pid_t int

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */

/* David Miller <davem@caip.rutgers.edu> says vfork fails on 2.4.  */
#undef HAVE_VFORK

#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib LD_SWITCH_X_SITE_AUX -R /usr/dt/lib
#else /* GCC */
/* We use ./prefix-args because we don't know whether LD_SWITCH_X_SITE_AUX
   has anything in it.  It can be empty.
   This works ok in src.  Luckily lib-src does not use LD_SWITCH_SYSTEM.  */
#define LD_SWITCH_SYSTEM -L /usr/ccs/lib \
 `./prefix-args -Xlinker LD_SWITCH_X_SITE_AUX` -R /usr/dt/lib
#endif /* GCC */

/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

/* Solaris 2.4 has a broken vfork.  And a header file has a declaration 
   that conflicts with the definition of vfork in sysdep.c.
   This definition should avoid it.  */
#define vfork emacs_vfork

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */

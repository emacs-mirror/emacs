/* Handle Solaris 2.4.  */

#include "sol2-3.h"

#define SOLARIS2_4

/* Solaris 2.4 has a broken vfork.  So we don't use it;
   we use the alternate definition in sysdep.c.
   But a header file has a declaration
   that would conflict with the definition of vfork in sysdep.c.
   So we'll choose the return type to match the system header.  */
#define VFORK_RETURN_TYPE pid_t

/* Get rid of -traditional and let const really do its thing.  */

#ifdef __GNUC__
#undef C_SWITCH_SYSTEM
#undef const
#endif /* __GNUC__ */

/* Handle Solaris 2.4.  */

#ifdef __GNUC__
#ifdef i386
/* Since we use gcc -traditional, we have to work around a bogus "volatile" in
   <sys/machlock.h>.  Must do this before #include "sol2-3.h".  */
#define volatile
#endif /* i386 */
#endif /* __GNUC__ */

#include "sol2-3.h"

#include "m-sequent.h"

#if 0 /* This is probably not necessary in GCC 2.2 and up.  */
/* When compiling with GCC and not optimizing,
   start has to skip one extra dummy word on the stack.  */

#if defined (__GNUC__) && !defined (__OPTIMIZE__)
#undef CRT0_DUMMIES
#define CRT0_DUMMIES dummy1, dummy2,
#endif
#endif /* 0 */

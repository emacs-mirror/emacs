/* Handle Solaris 2.6.  */

#include "sol2-5.h"

#if 0 /* dldump does not handle all the extensions used by GNU ld.  */
#undef UNEXEC
#define UNEXEC unexsol.o
#endif

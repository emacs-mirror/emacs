/* Handle Solaris 2.6.  */

#include "sol2-5.h"

#if 0 /* dldump does not handle all the extensions used by GNU ld.  */
#undef UNEXEC
#define UNEXEC unexsol.o
#endif

/* "Dennis McRitchie" <dmcr@Princeton.EDU> reported failures
   with GNU ld without this.  */

#define LD_SWITCH_SYSTEM_TEMACS -znocombreloc

/* arch-tag: 71ea3857-89dc-4395-9623-77964e6ed3ca
   (do not change this comment) */

/* Handle Solaris 2.10.  */

#include "sol2-6.h"

#define SYSTEM_MALLOC

/*
 * Use the Solaris dldump() function, called from unexsol.c, to dump
 * emacs, instead of the generic ELF dump code found in unexelf.c.
 * The resulting binary has a complete symbol table, and is better
 * for debugging and other observabilty tools (debuggers, pstack, etc).
 *
 * If you encounter a problem using dldump(), please consider sending
 * a message to the OpenSolaris tools-linking mailing list:
 *      http://mail.opensolaris.org/mailman/listinfo/tools-linking
 *
 * It is likely that dldump() works with older Solaris too,
 * but this has not been tested, and so, this change is for
 * Solaris 10 and newer only at this time.
 */
#undef UNEXEC
#define UNEXEC unexsol.o

/* This is used in list_system_processes.  */
#define HAVE_PROCFS 1

/* This is needed for the system_process_attributes implementation.  */
#define _STRUCTURED_PROC 1

/* arch-tag: 7c51a134-5469-4d16-aa00-d69224640eeb
   (do not change this comment) */

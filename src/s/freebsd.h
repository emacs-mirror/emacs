/* s/ file for freebsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "freebsd"

#undef KERNEL_FILE
#define KERNEL_FILE "/386bsd"

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "_averunnable"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil

#define HAVE_GETLOADAVG

/* For mem-limits.h.  */
#define BSD4_2

/* Shared library stuff. */
#define TEXT_START 0
#define DATA_START 0
#define UNEXEC unexfreebsd.o
#define RUN_TIME_REMAP
#define LINKER cc

/* Reread the time zone on startup.  */
#define LOCALTIME_CACHE

#define HAVE_TERMIOS
#define NO_TERMIO

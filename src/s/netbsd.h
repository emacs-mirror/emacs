/* s/ file for netbsd system.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "netbsd"

#undef KERNEL_FILE
#define KERNEL_FILE "/netbsd"

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "_averunnable"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define A_TEXT_OFFSET(x) (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define HAVE_SETSID

#define LIBS_DEBUG
#define LIBS_SYSTEM -lutil

#define HAVE_GETLOADAVG

/* For mem-limits.h.  */
#define BSD4_2

#define TERMCAP_NAME "/usr/share/misc/termcap"

#define SYSV_SYSTEM_DIR

/* These definitions should work for either dynamic or static linking,
   whichever is the default for `cc -nostdlib'.  */
#define BROKEN_START
#define TEXT_START ({ extern void start() asm ("start"); &start; })
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#define UNEXEC	unexsunos4.o
#define RUN_TIME_REMAP
#define N_PAGSIZ(x) __LDPGSZ
#define N_BSSADDR(x) (N_ALIGN(x, N_DATADDR(x)+x.a_data))
#define N_TRELOFF(x) N_RELOFF(x)

/* s/ file for bsd386 system.  */

#include "bsd4-3.h"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)
#define A_TEXT_OFFSET(x)    (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define LIBS_DEBUG
#define LIB_X11_LIB -L/usr/X11/lib -lX11
#define LIBS_SYSTEM -lutil -lkvm -lcompat

#define HAVE_GETLOADAVG

#undef BSD_PGRPS

/* The dumped Emacs records the timezone it was dumped in.  */
#define LOCALTIME_CACHE

/* System uses OXTABS instead of the expected TAB3.
   (Copied from netbsd.h.)  */
#define TABDLY OXTABS
#define TAB3 OXTABS

#define SYSV_SYSTEM_DIR

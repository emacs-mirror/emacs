/* s/ file for bsd386 system.  */

#include "bsd4-3.h"

#define SIGNALS_VIA_CHARACTERS

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)
#define A_TEXT_OFFSET(x)    (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define HAVE_SETSID

#define LIBS_DEBUG
#define LIB_X11_LIB -L/usr/X11/lib -lX11
#define LIBS_SYSTEM -lutil -lkvm -lcompat

#define HAVE_GETLOADAVG

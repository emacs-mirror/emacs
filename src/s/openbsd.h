/* s/ file for openbsd systems.  */

/* Get most of the stuff from bsd4.3 */
#include "bsd4-3.h"

/* Get the rest of the stuff from that less-POSIX-conformant system */
#include "netbsd.h"

/*  David Mazieres <dm@reeducation-labor.lcs.mit.edu> says this
    is necessary.  Otherwise Emacs dumps core when run -nw.  */
#undef LIBS_TERMCAP

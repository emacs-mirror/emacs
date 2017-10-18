/* Support SCO V 3.2.2 (also called Open Desk Top 1.1) */

#include "s-sco.h"

#ifdef HAVE_X11
/* Use Xselect instead of XSelect.  */
#undef select
#define select Xselect

/* This version was before random and srandom were added to libX11.a.  */
#undef HAVE_RANDOM

/* We don't have XrmDestroyDatabase in this version.  */
#define NO_X_DESTROY_DATABASE

/* Note: on ODT version 1.0, you need #undef BSTRING.  */

#endif /* HAVE_X11 */

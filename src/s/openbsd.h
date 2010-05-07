/* System file for openbsd.  */

/* Mostly the same as NetBSD.  */
#include "netbsd.h"

/*  David Mazieres <dm@reeducation-labor.lcs.mit.edu> says this
    is necessary.  Otherwise Emacs dumps core when run -nw.  */
#define TERMINFO
#undef LIBS_TERMCAP
#define LIBS_TERMCAP -lncurses

/* arch-tag: 7e3f65ca-3f48-4237-933f-2b208b21e8e2
   (do not change this comment) */

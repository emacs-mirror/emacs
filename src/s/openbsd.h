/* s/ file for openbsd systems.  */

/* Mostly the same as NetBSD.  */
#include "netbsd.h"

/*  David Mazieres <dm@reeducation-labor.lcs.mit.edu> says this
    is necessary.  Otherwise Emacs dumps core when run -nw.  */
#undef LIBS_TERMCAP

#define TERMINFO
#define LIBS_TERMCAP -lncurses

#undef LD_SWITCH_SYSTEM_TEMACS
#undef LD_SWITCH_SYSTEM
#ifdef __ELF__

/*  Han Boetes <han@mijncomputer.nl> says this
    is necessary,  otherwise Emacs dumps core on elf systems.  */
#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp -Z

#else

#define LD_SWITCH_SYSTEM LD_SWITCH_SYSTEM_tmp

#endif

/* arch-tag: 7e3f65ca-3f48-4237-933f-2b208b21e8e2
   (do not change this comment) */

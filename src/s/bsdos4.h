/* s/ file for BSDI BSD/OS 4.0 system.  */

#include "bsdos3.h"

/* BSD/OS 4.1 and later have ncurses */
#ifdef	HAVE_LIBNCURSES
#define	TERMINFO
#define	LIBS_TERMCAP -lncurses
#endif	/* HAVE_LIBNCURSES */

/* copied from freebsd.h */
#ifdef __ELF__

#define LD_SWITCH_SYSTEM
#undef START_FILES
#define START_FILES \
	pre-crt0.o /usr/lib/crt1.o /usr/lib/crti.o /usr/lib/crtbegin.o
#define UNEXEC unexelf.o
#define LIB_STANDARD -lgcc -lc -lgcc /usr/lib/crtend.o /usr/lib/crtn.o
#undef LIB_GCC
#define LIB_GCC

#endif /* not __ELF__ */

/* arch-tag: 7659632a-a879-4153-bb8b-3765a1463ca1
   (do not change this comment) */

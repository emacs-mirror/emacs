/* Definitions for ESIX, a variant of v.5.3 for the 386.  */
/* These are based on reports for ESIX 5.3.2 D.  */

#include "usg5-3.h"

/* Some versions of V.3 have this, but not all. ESIX does. */
#define HAVE_PTYS
#define SYSV_PTYS

/* Have -lg be used for debugging. */
#undef LIBS_DEBUG
#define LIBS_DEBUG -lg

/* If using Roell's X server, define X11R4 */
#ifdef X11R4			/* Roell's X server */
#define select sys_select /* Emacs select() not good enough? */
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -lpt
#endif /* X11R4 */

/* ESIX does not need <sys/sioctl.h>, but needs <sys/ptem.h> */
#define NO_SIOCTL_H
#define NEED_PTEM_H
#define BROKEN_FIONREAD

/* arch-tag: d66e6a70-0812-437d-aa35-9b01bd474941
   (do not change this comment) */

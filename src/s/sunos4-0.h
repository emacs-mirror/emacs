#include "bsd4-2.h"

#ifndef SUNOS4
#define SUNOS4
#endif

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

/* We use the Sun syntax -Bstatic unconditionally, because even when we
   use GCC, these are passed through to the linker, not handled by GCC
   directly.  */
#define LD_SWITCH_SYSTEM -Bstatic

/* We use this for linking temacs, but not for other programs
   or for tests in configure.  */
#define LD_SWITCH_SYSTEM_TEMACS -e __start

/* In SunOS 4.1, a static function called by tzsetwall reportedly
   clears the byte just past an eight byte region it mallocs, corrupting
   GNU malloc's memory pool.  But Sun's malloc doesn't seem to mind. */

#define SYSTEM_MALLOC

#ifdef __GNUC__
/* We must define mkdir with this arg prototype
   to match GCC's fixed stat.h.  */
#define MKDIR_PROTOTYPE \
  int mkdir (const char *dpath, unsigned short dmode)
#endif /* __GNUC__ */

#include "bsd4-2.h"

#if 0  /* This may have been needed for an earlier version of Sun OS 4.
	  It seems to cause warnings in 4.0.3 and 4.1.  */
#define O_NDELAY        FNDELAY /* Non-blocking I/O (4.2 style) */
#endif

#ifdef __GNUC__
#define LD_SWITCH_SYSTEM -e __start -static
#else
#define LD_SWITCH_SYSTEM -e __start -Bstatic
#endif

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

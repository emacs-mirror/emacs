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

/* SunOS 4.x cc <stdlib.h> declares abort and free to return int.  */

#ifndef __STDC__
#define ABORT_RETURN_TYPE int
#define FREE_RETURN_TYPE int
#endif

#ifdef __GNUC__
/* We must define mkdir with this arg prototype
   to match GCC's fixed stat.h.  */
#define MKDIR_PROTOTYPE \
  int mkdir (const char *dpath, unsigned short dmode)
#endif /* __GNUC__ */

/* Must use the system's termcap, if we use any termcap.
   It does special things.  */

#ifndef TERMINFO
#define LIBS_TERMCAP -ltermcap
#endif

#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS

/* arch-tag: 362f3bfc-810d-4f6e-9b83-5a32f8f1a926
   (do not change this comment) */

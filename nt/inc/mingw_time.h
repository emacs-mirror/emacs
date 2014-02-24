/* Fix issues related to MinGW 4.x handling of time data types.  */

#ifndef _MINGW_TIME_H
#define _MINGW_TIME_H

/* The @#$%^&! MinGW developers stopped updating the values of
   __MINGW32_VERSION, __MINGW32_MAJOR_VERSION, and
   __MINGW32_MINOR_VERSION values in v4.x of the runtime, to
   "discourage its uses".  So the values of those macros can no longer
   be trusted, and we need the workaround below, to have a single set
   of macros we can trust.  (The .17 minor version is arbitrary.)  */
#ifdef __MINGW32__
#include <_mingw.h>
#endif
/* MinGW64 doesn't have this problem, and does not define
   __MINGW_VERSION.  */
#ifndef __MINGW64_VERSION_MAJOR
# ifndef __MINGW_VERSION
#  define __MINGW_VERSION 3.17
#  undef __MINGW_MAJOR_VERSION
#  define __MINGW_MAJOR_VERSION 3
#  undef __MINGW_MINOR_VERSION
#  define __MINGW_MINOR_VERSION 17
#  undef __MINGW_PATCHLEVEL
#  define __MINGW_PATCHLEVEL 0
# endif
#endif
#if __MINGW_MAJOR_VERSION >= 4
# define _USE_32BIT_TIME_T
#endif

#endif

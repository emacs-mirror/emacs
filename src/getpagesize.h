#ifdef BSD
#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
#endif

#ifndef HAVE_GETPAGESIZE

#ifdef VMS
#include "param.h"
#else
#include <sys/param.h>
#endif

#ifdef POSIX
#include <unistd.h>
#define getpagesize() sysconf (_SC_PAGESIZE)
#else /* not POSIX */
#ifdef EXEC_PAGESIZE
#define getpagesize() EXEC_PAGESIZE
#else /* no EXEC_PAGESIZE */
#ifdef NBPG
#define getpagesize() NBPG * CLSIZE
#ifndef CLSIZE
#define CLSIZE 1
#endif /* no CLSIZE */
#else /* no NBPG */
#define getpagesize() NBPC
#endif /* no NBPG */
#endif /* no EXEC_PAGESIZE */
#endif /* not POSIX */

#endif /* not HAVE_GETPAGESIZE */


/* Include the appropriate header files for things related to time.
   The caller should include sys/socket.h before this file,
   if the system has sockets.   */

#ifdef IRIS
#include <sys/sysmacros.h>	/* for "minor" */
#include <sys/time.h>
#else
#ifdef UNIPLUS
#include <sys/time.h>

#else /* not IRIS, not UNIPLUS */
#ifdef HAVE_TIMEVAL
/* _h_BSDTYPES is checked because on ISC unix, socket.h includes
   both time.h and sys/time.h, and the latter file is protected
   from repeated inclusion.  */
#if defined(USG) && !defined(AIX) && !defined(_h_BSDTYPES) && !defined(USG_SYS_TIME) && !defined(uts)
#include <time.h>
#else /* AIX or USG_SYS_TIME, or not USG */
#include <sys/time.h>
#endif /* AIX or USG_SYS_TIME, or not USG */
#endif /* HAVE_TIMEVAL */
#endif /* not UNIPLUS */
#endif /* not IRIS */

#ifdef BAT68K
#include <sys/time.h>   /* In addition to time.h.  */
#endif

#ifdef AIX
#include <sys/time.h>   /* In addition to time.h.  */
#endif

#ifdef DPX2
#include <sys/time.h>
#endif

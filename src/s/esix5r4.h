/* Definitions for ESIX System V 4.0.4, a variant of V.4 for the 386.  */
/* Redone by zircon!joe@uunet.uu.net (Joe Kelsey).  */

#include "usg5-4.h"

#define SYSTEM_MALLOC 1
#if defined (HAVE_XFREE386)
# undef LIB_STANDARD
# define LIB_STANDARD -lc
#else
# define LIB_X11_LIB -lsocket -lc -lX11
# undef LIB_STANDARD
# ifdef ORDINARY_LINK
#   define LIB_STANDARD -lnsl -lns -lelf /usr/ucblib/libucb.a
# else
#   define LIB_STANDARD -lnsl -lns -lelf /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o
# endif

/* Resolve BSD string functions in X Window library from libucb.a.  */
# define BSTRING

/* zircon!joe says this makes X windows work.  */
# define BROKEN_FIONREAD
#endif

/* arch-tag: 2d314ae9-0357-4ddf-96e5-cf821071ba4b
   (do not change this comment) */

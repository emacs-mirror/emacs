#include "s-usg5-3.h"

#ifndef ISC
#define ISC
#endif

#define NO_SIOCTL_H

#define BROKEN_TIOCGETC

/* There are some reports that the following is needed
   with some version of this system.
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM -linet

#define HAVE_TIMEVAL
#define USE_UTIME
*/

/* This is said to be needed as a result of having _insque rather
   than insque in -loldX.  This may not always be the right thing.  */
#define WRONG_NAME_INSQUE

/* Definitions for ESIX System V 4.0.4, a variant of V.4 for the 386.  */

#include "usg5-4.h"

#define LIB_X11_LIB -lsocket -lc -lX11
#undef LIB_STANDARD
#define LIB_STANDARD -lnsl -lns -lelf /usr/ucblib/libucb.a /usr/ccs/lib/crtn.o

/* Resolve BSD string functions in X Window library from libucb.a.  */
#define BSTRING

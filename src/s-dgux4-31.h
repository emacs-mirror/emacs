#include "s-dgux.h"

/* No libdgc.a in dgux version 4.31.  */
#undef LIBS_SYSTEM
#define LIBS_SYSTEM /lib/crt0.o

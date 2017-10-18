#include "s-usg5-4.h"

/* cracraft@ai.mit.edu says these work.  */
#define BROKEN_FIONREAD
#undef SYSV_STREAMS

#define POSIX

/* Here is how to find X Windows.  */
#define LD_SWITCH_SYSTEM -L/usr/openwin/lib
#define C_SWITCH_X_SYSTEM -I/usr/openwin/include

/* Compile in non-ansi fashion to work around bugs in system header files.  */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Xs
#else /* GCC */
#define C_SWITCH_SYSTEM -traditional
#endif /* GCC */

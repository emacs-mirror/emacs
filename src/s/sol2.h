#include "s-usg5-4.h"

#define POSIX

/* Here is how to find X Windows.  The -L option tells the linker where
   to find the libraries at link time, the -R option at run time.  */
#define LD_SWITCH_SYSTEM -L/usr/openwin/lib -R/usr/openwin/lib
#define C_SWITCH_X_SYSTEM -I/usr/openwin/include

/* Compile in non-ansi fashion to work around bugs in system header files.  */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Xs
#else /* GCC */
#define C_SWITCH_SYSTEM -traditional
#endif /* GCC */

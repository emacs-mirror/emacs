#include "s-usg5-4.h"

#define POSIX

/* Here is how to find X Windows.  The -L option tells the linker where
   to find the libraries at link time, the -R option at run time.  */
#define C_SWITCH_X_SYSTEM -I/usr/openwin/include
#ifndef __GNUC__
#define LD_SWITCH_SYSTEM -L/usr/openwin/lib -R/usr/openwin/lib
#else /* GCC */
#define LD_SWITCH_SYSTEM -L/usr/openwin/lib -Xlinker -R/usr/openwin/lib
#endif /* GCC */

/* Compile in non-ansi fashion to work around bugs in system header files.  */
#ifndef __GNUC__
#define C_SWITCH_SYSTEM -Xs
#else /* GCC */
#define C_SWITCH_SYSTEM -traditional
#endif /* GCC */

/* Karl Berry writes:
If you have the misfortune to be running Solaris 2.1, you may have
noticed that the access system call does not check the readonlyness of
the filesystem the path refers to.  This is a bug, according to
access(2), but in the meantime, some of us need the right behavior.  */
#define SOLARIS_BROKEN_ACCESS

/* s- file for building Emacs on AIX 3.2.5.  */

#include "aix3-2.h"

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#undef C_OPTIMIZE_SWITCH
#define C_DEBUG_SWITCH -O
#define C_OPTIMIZE_SWITCH -O
#endif

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#define SIGNALS_VIA_CHARACTERS
#define MAIL_USE_LOCKF
#define CLASH_DETECTION

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says these are correct.  */
#define POSIX_SIGNALS
#undef sigmask
#undef sigsetmask
#undef _setjmp
#undef _longjmp

/* Bill Woodward <wpwood@austin.ibm.com> says:
   libIM *must* precede libXm, to avoid getting aixLoadIM error messages.  */
#define LIB_MOTIF -lIM -lXm

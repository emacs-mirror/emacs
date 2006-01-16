/* s- file for building Emacs on AIX 3.2.5.  */

#include "aix3-2.h"

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
/* The above isn't generally true.  If it occurs with some compiler
   release, seek a fixed version, be it XLC or GCC.  The XLC version
   isn't tied to the OS version on AIX any more than elsewhere.  XLC
   (the IBM compiler) can use -g with -O.  (-O3 is also a possibility
   for the optimization level.)  -- fx, after David Edelsohn.  */
#undef C_DEBUG_SWITCH
#define C_DEBUG_SWITCH -g -O

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

/* arch-tag: 692b3acb-5383-4cfb-93f6-378b5c48c75e
   (do not change this comment) */

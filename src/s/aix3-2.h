/* s- file for building Emacs on AIX 3.2.  */

#include "aix3-1.h"

#define AIX3_2

/* No need to define this--the header files indicate X11R4,
   and that's supposedly what 3.2 will come with.  */
#undef SPECIFY_X11R4

#ifndef __GNUC__
/* Some programs in src produce warnings saying certain subprograms
   are to comples and need a MAXMEM value greater than 2000 for
   additional optimization.  --nils@exp-math.uni-essen.de */
#define C_SWITCH_SYSTEM -ma -qmaxmem=4000
#endif

/* Adrian Colley <Adrian.Colley@three.serpentine.com> says this is needed.  */
#ifndef NOT_C_CODE
#ifndef AIX4
 #pragma alloca
#endif
#endif

#undef rindex
#undef index

#define HAVE_FSYNC

/* With this defined, a gcc-compiled Emacs crashed in realloc under AIX
   3.2, and a cc-compiled Emacs works with this undefined.
   --karl@cs.umb.edu.  */
#undef SYSTEM_MALLOC

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes. Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu */
/* See comments about this in aix3-2-5.h.  -- fx */
#ifndef __GNUC__
#define C_DEBUG_SWITCH -g -O
#endif

/* The character-composition stuff is broken in X11R5.
   Even with XIMStatusNothing aliased to XIMStatusNone,
   tranle@intellicorp.com (Minh Tran-Le) reports that enabling
   the internationalization code causes the modifier keys C, M and Shift
   to beep after a mouse click.  */
#define X11R5_INHIBIT_I18N

/* string.h defines rindex as a macro, at least with native cc, so we
   lose declaring char * rindex without this.
   It is just a guess which versions of AIX need this definition.  */
#undef HAVE_STRING_H

#include "aix4-1.h"

#undef ALIGN_DATA_RELOC

/* On AIX Emacs uses the gmalloc.c malloc implementation.  But given
   the way this system works, libc functions that return malloced
   memory use the libc malloc implementation. Calling xfree or
   xrealloc on the results of such functions results in a crash. 

   One solution for this could be to define SYSTEM_MALLOC here, but
   that does not currently work on this system.

   It is possible to completely override the malloc implementation on
   AIX, but that involves putting the malloc functions in a shared
   library and setting the MALLOCTYPE environment variable to point to
   tha shared library.
   
   Emacs currently calls xrealloc on the results of get_current_dir name,
   to avoid a crash just use the Emacs implementation for that function.
*/
#define BROKEN_GET_CURRENT_DIR_NAME 1

/* aix3-1.h defined _NO_PROTO, probably to work around an AIX compiler
   that did not handle prototypes.  On (at least) AIX 5.2, this causes
   the proper prototype to be thrown away for lseek64, so compiled
   Lisp files do not load correctly and compilation fails.

   The AIX compiler should have learned about function prototypes long
   ago, so we can probably go ahead and undefine _NO_PROTO.  However,
   if someone can demonstrate that this problem still exists for AIX
   4, this should be moved into a new file (aix5.h).
*/

#undef _NO_PROTO


/* arch-tag: 38fe75ea-6aef-42bd-8449-bc34d921a562
   (do not change this comment) */

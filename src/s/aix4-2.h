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

/* arch-tag: 38fe75ea-6aef-42bd-8449-bc34d921a562
   (do not change this comment) */

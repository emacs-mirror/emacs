/* Handle Solaris 2.5.  */

#include "sol2-4.h"

/* -lgen is needed for the regex and regcmp functions
   which are used by Motif.  In the future we can try changing
   regex.c to provide them in Emacs, but this is safer for now.  */
#define LIB_MOTIF -lXm -lgen

/* This is the only known way to avoid some crashes
   that seem to relate to screwed up malloc data
   after deleting a frame.  */
/* rms: I think the problems using ralloc had to do with system
   libraries that called the system malloc even if we linked in the
   GNU malloc.  I could not see any way to fix the problem except to
   have just one malloc and that had to be the system one.  */
/* This is not always necessary.  Turned off at present for testers to
   identify any problems with gmalloc more accurately.  */
/* #define SYSTEM_MALLOC */

/* There have problems reported with mmap at least on Solaris 2.6
   and 2.7.  For simplicity, let's not use mmap for anything >= 2.5.
   We can optimize this later.  */

#undef USE_MMAP_FOR_BUFFERS

/* This prevents crashes in "M-x shell".  */
#ifndef HAVE_VFORK
#define HAVE_VFORK 1
#endif

/* Newer versions of Solaris have bcopy etc. as functions, with
   prototypes in strings.h.  They lose if the defines from usg5-4.h
   are visible, which happens when X headers are included.  */
#ifdef HAVE_BCOPY
#undef bcopy
#undef bzero
#undef bcmp
#ifndef NOT_C_CODE
#include <strings.h>
#endif
#endif

#if 0 /* A recent patch in unexelf.c should eliminate the need for this.  */
/* Don't use the shared libraries for -lXt and -lXaw,
   to work around a linker bug in Solaris 2.5.
   (This also affects the other libraries used specifically for
   the X toolkit, which may not be necessary.)  */
#define LIBXT_STATIC

#ifdef __GNUC__
#define STATIC_OPTION -Xlinker -Bstatic
#define DYNAMIC_OPTION -Xlinker -Bdynamic
#else
#define STATIC_OPTION -Bstatic
#define DYNAMIC_OPTION -Bdynamic
#endif

#endif /* 0 */

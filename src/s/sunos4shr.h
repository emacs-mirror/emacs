/* This file permits building Emacs with a shared libc on Sunos 4.
   To make this work, you must completely replace your C shared library
   using one of the SunOS 4.1.x jumbo replacement patches from Sun.
   Here are the patch numbers for Sunos 4.1.3:
   100890-10   SunOS 4.1.3: domestic libc jumbo patch
   100891-10   SunOS 4.1.3: international libc jumbo patch  */


#include "sunos4-1.h"

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

/*  Misleading!  Actually gets loaded after crt0.o */
#define START_FILES pre-crt0.o

/*
 *  Kludge!  can't get at symbol "start" in std crt0.o
 *  Who the #$%&* decided to remove the __ characters!
 *  Someone needs to fix this in sysdep.c  with an #ifdef BROKEN_START in
 * sysdep.c.  We do not use this address so any value should do really.  Still
 *  may need it in the future?
 */
#define BROKEN_START
#define TEXT_START 0x2020

#define UNEXEC	unexsunos4.o
#define RUN_TIME_REMAP
#define ORDINARY_LINK

#undef LD_SWITCH_SYSTEM

#undef	SYSTEM_MALLOC
#ifndef GNU_MALLOC
#define	GNU_MALLOC
#endif
#ifndef REL_ALLOC
#define	REL_ALLOC
#endif

/* khera@cs.duke.edu says this is needed.  */
#define memmove(to, from, size) bcopy (from, to, size)

#undef USE_DL_STUBS

#ifdef __GNUC__
#define LIBXMU -Xlinker -Bstatic -lXmu -Xlinker -Bdynamic
#else
#define LIBXMU -Bstatic -lXmu -Bdynamic
#endif

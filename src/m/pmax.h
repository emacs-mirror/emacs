/* Machine description file for DEC MIPS machines.  */

#include "mips.h"

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"  

NOTE-START
Use -opsystem=osf1 for OSF/1, and -opsystem=bsd4-3 otherwise.
NOTE-END  */

#undef WORDS_BIG_ENDIAN
#undef LIB_STANDARD
#undef START_FILES
#undef COFF
#undef TERMINFO
#define MAIL_USE_FLOCK
#define HAVE_UNION_WAIT


#ifdef MACH
#define START_FILES pre-crt0.o /usr/lib/crt0.o
#else
/* This line starts being needed with ultrix 4.0.  */
/* You must delete it for version 3.1.  */
#define START_FILES pre-crt0.o /usr/lib/cmplrs/cc/crt0.o
#endif

/* Supposedly the following will overcome a kernel bug.  */
#undef LD_SWITCH_MACHINE
#undef DATA_START
#define DATA_START 0x10000000
#define DATA_SEG_BITS 0x10000000

#if 0
/* I don't see any such conflict in Ultrix 4.2, 4.2a, or 4.3.  And
   the relocating allocator is a real win.  -JimB  */

/* In Ultrix 4.1, XvmsAlloc.o in libX11.a seems to insist
   on defining malloc itself.  This should avoid conflicting with it.  */
#define SYSTEM_MALLOC
#endif

/* Override what mips.h says about this.  */
#undef LINKER

/* Ultrix 4.2 (perhaps also 4.1) implements O_NONBLOCK
   but it doesn't work right;
   and it causes hanging in read_process_output.  */
#define BROKEN_O_NONBLOCK

#if defined (OSF1) || defined (MACH)
#undef C_ALLOCA
#define HAVE_ALLOCA
#endif

#ifndef OSF1
/* Ultrix saves the time zone in core; must clear it.  */
#define LOCALTIME_CACHE
#endif

/* mcc@timessqr.gc.cuny.edu says this makes Emacs work with DECnet.  */
#ifdef HAVE_LIBDNET
#define LIBS_MACHINE -ldnet
#endif

/* mcc@timessqr.gc.cuny.edu says it is /vmunix on Ultrix 4.2a.  */
#undef KERNEL_FILE
#define KERNEL_FILE "/vmunix"

#ifndef MACH
/* Jim Wilson writes:
   [...] The X11 include files that Dec distributes with Ultrix
   are bogus.

   When __STDC__ is defined (which is true with gcc), the X11 include files
   try to define prototypes.  The prototypes however use types which haven't
   been defined yet, and thus we get syntax/parse errors.

   You can not fix this by changing the include files, because the prototypes
   create circular dependencies, in particular Xutil.h depends on types defined
   in Xlib.h, and Xlib.h depends on types defined in Xutil.h.  So, no matter
   which order you try to include them in, it will still fail.

   Compiling with -DNeedFunctionPrototypes=0 will solve the problem by
   directly inhibiting the bad prototypes.  This could perhaps just be put in
   an a Ultrix configuration file.

   Using the MIT X11 distribution instead of the one provided by Dec will
   also solve the problem, but I doubt you can convince everyone to do this. */
/* Addendum: the MIT X11 distribution neglects to define certain symbols
   when NeedFunctionPrototypes is 0, but still tries to use them when
   NeedVarargsProrotypes is 1 (which is its default value).  So if we're
   going to disable non-variadic prototypes, we also need to disable
   variadic prototypes.  --kwzh@gnu.ai.mit.edu */
#define C_SWITCH_X_MACHINE -DNeedFunctionPrototypes=0 -DNeedVarargsPrototypes=0
#endif

/* Enable a fix in process.c.  */
#define SET_CHILD_PTY_PGRP

/* m- file for the Tandem Integrity S2.  */

#include "m-mips.h"

/* This overrides some of the usual support for the mips and system V.3.  */

/* Comment this out if using NonStop-UX OS rev. below A10. */
#define A10

/* If not using X11R4, then comment out. /usr/lib/libX11.a has */
 /* globally define bcopy, bcmp, bzero, random & srandom that conflicts */
 /* with GNU's. You won't need this for X11R3 .*/
#define USE_X11R4

/* START_FILES and LIB_STANDARD are located in different places for
   'C' version 2.10 */
#ifdef A10
#ifdef START_FILES
#undef START_FILES
#define START_FILES pre-crt0.o /lib/crt1.o
#endif

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#define LIB_STANDARD -lc /lib/crtn.o
#endif
#else        /* Revision below A10 */
#ifdef START_FILES
#undef START_FILES
#define START_FILES pre-crt0.o /usr/lib/crt1.o
#endif

#ifdef LIB_STANDARD
#undef LIB_STANDARD
#define LIB_STANDARD -lc /usr/lib/crtn.o
#endif
#endif     /* End #ifdef A10 vs not */

/* Use X11R4's bcopy, bmem,bzero, random & srandom rather than GNU's */
 /* which causes multiply-defined problems when -lX11 is loaded. */
#ifdef USE_X11R4
#define HAVE_RANDOM /* Use X's random */
#define BSTRING     /* Use X's bcopy, bmem and bzero */
#endif

/* The S2 does not know about utimes() */
#define USE_UTIME

/* The operating system apparently defines TIOCGETC
   but it doesn't work.  */
#undef BROKEN_TIOCGETC

/* Change LIBS_TERMCAP (from m-mips.h) to ensure that libbsd.a is loaded
   before libcurses.a.  The curses library has its own version of select(2)
   which does not work with GNU Emacs; libbsd.a has the right version.

   We also use the malloc(3X) package in place of both malloc(3C) in libc.a
   and GNU malloc.  The GNU malloc is not working correctly for large files
   (2MB and up), and malloc(3X) handles this better than malloc(3C). */
#ifdef LIBS_TERMCAP
#undef LIBS_TERMCAP
#define LIBS_TERMCAP -lmalloc -lbsd -lcurses
#endif

#define SYSTEM_MALLOC

/* Note that src/ymakefile should also be changed to load LIBS_TERMCAP _after_
   $(LIBX), since libX11.a has its own version of writev which should override
   the version in libbsd.a. */

/* Emacs can use the NonStop-UX select(2) to support subprocesses and X11.
   Note that the correct version of select(2) is in libbsd.a; see above. */
#define HAVE_SELECT

/* Subprocesses now work because we are using the correct select(2) call.
   The reason it didn't work before was because curses with its select call
   was being loaded before the correct select(2) which is in libbsd.a. */
#ifndef subprocesses
#define subprocesses
#endif

/* The gettimeofday(2) routine is present in libbsd.a but is not supported
   or documented. */
#undef HAVE_GETTIMEOFDAY

/* Remove definition of LIBX11_SYSTEM (from s-usg5-3.h).  It references
   a library -lnsl_s that is not present and not required on the S2. */
#ifdef LIBX11_SYSTEM
#undef LIBX11_SYSTEM
#endif

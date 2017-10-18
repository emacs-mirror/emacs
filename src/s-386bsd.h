/* Definitions file for GNU emacs running on 386BSD.  */

#include "s-bsd4-3.h"

/*
 *     Define symbols to identify the version of Unix this is.
 *     BSD and BSD4_3 are defined in s-bsd4-3.h.  There are some
 *     differences between 386BSD and BSD 4.3 so we need an extra
 *     symbol to identify it (the J stands for Jolitz).
 */

#ifndef J386BSD
#define J386BSD
#endif /* J386BSD */

/* Under 386BSD the file containing the kernel's symbol 
   table is named /386bsd.  */

#undef KERNEL_FILE
#define KERNEL_FILE "/386bsd"

/* The symbol in the kernel where the load average is found
   is named _averunnable.  */

#undef LDAV_SYMBOL
#define LDAV_SYMBOL "_averunnable"

/* This macro determines the number of bytes waiting to be written
   in a FILE buffer.  */

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* 386BSD uses GNU C.  */

#define C_COMPILER gcc -traditional

/* 386BSD stores the termcap database in /usr/share/misc rather than
   /etc. We use the system termcap library to avoid putting a #ifdef
   in termcap.c or forcing the user to use the TERMCAP environment
   variable.  */

#define LIBS_TERMCAP -ltermcap

/* 386BSD is nominally a POSIX.1 OS and has setsid.  */

#ifndef HAVE_SETSID
#define HAVE_SETSID
#endif /* HAVE_SETSID */

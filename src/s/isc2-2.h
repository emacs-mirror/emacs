/* system description file for Interactive (ISC) Unix version 2.2 on
   the 386.  */

#include "usg5-3.h"

/* With this defined, subprocesses made by (e.g.) M-x grep don't exit
   cleanly, they just hang.  ISC 2.2.1 does have select, in the -linet
   library, but I guess it's not what Emacs needs.  --karl@cs.umb.edu
#define HAVE_SELECT */

/* Although ISC has sockets, again in -linet, again it's not what Emacs
   needs.  With this defined, interrupt-shell-subjob and the like do
   nothing.  --karl@cs.umb.edu
#define HAVE_SOCKETS */


/* This keeps the .cdbx section that gcc puts out when generating
   stabs-in-coff output, so Emacs can be debugged.  --karl@cs.umb.edu*/
#define USG_SHARED_LIBRARIES

/* We can support lock files.  */
#define CLASH_DETECTION
#define NO_FCHMOD

#define HAVE_PTYS
#define HAVE_CLOSEDIR
#define MAXNAMLEN 512
#define O_NDELAY O_NONBLOCK
#define MEMORY_IN_STRING_H

/* -lcposix is always needed for rename.  -lPW is only needed if not gcc.  */
#ifdef __GNUC__
#define LIB_STANDARD -lcposix -lc
#else
#define LIB_STANDARD -lPW -lcposix -lc
#endif

/* mt00@etherm.co.uk says this is needed for process.c.  */
#define USE_UTIME

#define NO_X_DESTROY_DATABASE

/* This communicates with m-intel386.h.  */
#define DONT_DEFINE_SIGNAL

/* May be needed to avoid undefined symbols such as gethostname,
   inet_addr, gethostbyname, socket, connect, ...  But if we are not
   compiling with X support, it's not needed.  */
#ifdef HAVE_X_WINDOWS
#define LIBS_SYSTEM -linet
#endif

/* This system has job control.  */
#undef NOMULTIPLEJOBS

/* Inhibit asm code in netinet/in.h.  Strictly speaking, only necessary
   when -traditional is being used, but it doesn't hurt to
   unconditionally define this.  */
#define NO_ASM

/* The POSIX-ified system headers don't work with GCC.  */
#ifdef __GNUC__
#define C_SWITCH_SYSTEM -traditional
#endif

/* Some versions of ISC are said to define S_IFLNK even tho
   they don't really support symlinks.  */
#undef S_IFLNK

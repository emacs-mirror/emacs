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
   stabs-in-coff output, so Emacs can be debugged.  --karl@cs.umb.edu. */
#define USG_SHARED_LIBRARIES

/* We can support lock files.  */
#define CLASH_DETECTION
#define NO_FCHMOD

#define HAVE_PTYS
#define MAXNAMLEN 512
#define O_NDELAY O_NONBLOCK
#define MEMORY_IN_STRING_H

/* Tell gmalloc.c that we don't have memmove (system include files to the
   contrary!). */
#define MEMMOVE_MISSING

/* Send a signal to a subprocess by "typing" a signal character. */
#define SIGNALS_VIA_CHARACTERS

/* -lPW is only needed if not using Gcc.  We used to include -lcposix here
   for the rename function, but some pepople saus ISC's renames doesn't
   work correctly with Emacs so we use Emacs' emulation instead. */
#if defined (__GNUC__)
#  define LIB_STANDARD -lcposix -lc
#else /* !__GNUC__ */
#  define LIB_STANDARD -lPW -lc
#endif /* !__GNUC__ */

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

/* -traditional is not necessary if the system header files are fixed to
   define getc and putc in the absence of _POSIX_SOURCE.  GCC's from 2.4.4
   on do this. */
#if !defined (__GNUC__) || __GNUC__ < 2
#  define C_SWITCH_SYSTEM -traditional
#endif

/* Some versions of ISC are said to define S_IFLNK even tho
   they don't really support symlinks.  */
#undef S_IFLNK

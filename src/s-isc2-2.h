/* s- file for Interactive (ISC) Unix version 2.2 on the 386.  */

#include "s-usg5-3.h"

#define HAVE_SOCKETS
#define HAVE_SELECT
#define HAVE_PTYS
#define HAVE_RENAME
#define HAVE_CLOSEDIR
#define MAXNAMLEN 512
#define LIB_STANDARD -lPW -lcposix -lc
#define O_NDELAY O_NONBLOCK
#define MEMORY_IN_STRING_H

/* ball@seal.witchcraft.com says suspending works ok in 18.58.  */
/* #undef SIGTSTP */

/* This communicates with m-intel386.h.  */
#define DONT_DEFINE_SIGNAL

/* May be needed to avoid undefined symbols
   such as gethostname, inet_addr, gethostbyname, socket, connect... */
/* ??? There is a suggestion that -lpt is needed here.
   If you have trouble compiling wiht HAVE_X_WINDOWS, please try that.  */
#define LIBS_SYSTEM -linet

/* This system has job control.  */
#undef NOMULTIPLEJOBS

/* Send a signal to a subprocess by "typing" a signal character.  */
#define SIGNALS_VIA_CHARACTERS
#define TIOCGPGRP (TIOC|21) /* From termio.h.  */

#if 0
/* Some files need types.h to link properly.  */
#ifndef NO_SHORTNAMES /* Don't do this in ymakefile!  */
#include <sys/types.h>
#endif
#endif

#if 0 /* These might be needed if you compile with `gcc -posix'.
	 It's not certain.  */
/* I'm not sure under what circumstances this file is needed, but both
   gcc and cc link with it by default, so... */
#define OBJECTS_SYSTEM /lib/crtn.o

/* Link with POSIX runtime if we are compiling in the POSIX environment.  */
#ifdef _POSIX_SOURCE
#define START_FILES pre-crt0.o /lib/crtp0.o
#endif
#endif

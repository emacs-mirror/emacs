/* system description file for Interactive (ISC) Unix version 2.2 on
   the 386.  */

#include "usg5-3.h"

#define HAVE_PTYS
#define HAVE_RENAME
#define HAVE_CLOSEDIR
#define MAXNAMLEN 512
#define LIB_STANDARD -lcposix -lc
#define O_NDELAY O_NONBLOCK
#define MEMORY_IN_STRING_H
#undef SIGTSTP

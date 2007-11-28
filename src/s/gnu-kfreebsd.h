#include "gnu-linux.h"

/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu/kfreebsd" /* All the best software is free */

#define NO_TERMIO               /* use only <termios.h> */

/* arch-tag: 8d098200-2586-469e-99ab-6d092c035e03
   (do not change this comment) */

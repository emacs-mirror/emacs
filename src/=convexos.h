/* Definitions file for GNU Emacs running on ConvexOS.  */

#include "bsd4-3.h"

/* First pty name is /dev/pty?0.  We have to search for it. */
#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER first_pty_letter

/* getpgrp requires no arguments. */
#define GETPGRP_NO_ARG

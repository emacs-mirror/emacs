#include "hpux10.h"

#define HPUX11

#ifdef POSIX_SIGNALS
#undef POSIX_SIGNALS
#endif
#define POSIX_SIGNALS 1

/* SA_RESTART resets the timeout of `select', so don't use it.  */
#define BROKEN_SA_RESTART

/* It does work on HPUX to open the pty's tty in the parent (Emacs),
   then close and reopen it in the child.  */
#define USG_SUBTTY_WORKS

/* HPUX 10.10 needs this; HPUX 10.20 does not, and HPUX 11 does not.  */
#undef POLLING_PROBLEM_IN_SELECT

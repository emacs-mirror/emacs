/* As of 19.29, it should work ok to use shared libs with X.  */
#include "sunos4shr.h"

#if 0
#include "sunos4-1.h"

/* jik@gza.com says this works now.  */
/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC

#if 0 /* This causes failure in process_send_signal (tcgetattr loses)
	 and may also cause hanging at Emacs startup when parent
	 is not a job control shell.  */
/* murray@chemical-eng.edinburgh.ac.uk says this works, and avoids
   the problem of spurious ^M in subprocess output.  */
#define HAVE_TERMIOS
/* This enables some #undefs in systty.h.  */
#define BSD_TERMIOS
#endif

/* barrie@calvin.demon.co.uk says memmove is missing.  */
#ifndef SYSTEM_MALLOC
#define MEMMOVE_MISSING
#endif
#endif /* 0 */

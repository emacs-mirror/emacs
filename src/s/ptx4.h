/* s/ file for Sequent "ptx 4", which is a modified SVR5.4.  */

/* Tell usg5-4.h not to include filio.h.  */
#define NO_FILIO_H

#include "usg5-4.h"

/* Marcus Daniels <marcus@sysc.pdx.edu> says that SIGINFO is defined
   on ptx4 but it is not a signal.  Prevent process.c from doing the
   wrong thing.  */
#undef SIGINFO

/* Marcus Daniels <marcus@sysc.pdx.edu> says vfork does exist.  */
#define HAVE_VFORK

/* pae@dim.com (Phil Ernhardt) says this correction to
   the definition in usg5-4.h is needed to prevent
   all asynchronous subprocesses from exiting right away.  */
#undef SETUP_SLAVE_PTY
#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1) \
    fatal ("ioctl I_PUSH ttcompat", errno);

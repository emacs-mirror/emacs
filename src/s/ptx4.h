/* s/ file for Sequent "ptx 4", which is a modified SVR5.4.  */

/* Tell usg5-4.h not to include filio.h.  */
#define NO_FILIO_H

#include "usg5-4.h"

/* Marcus Daniels <marcus@sysc.pdx.edu> says that SIGINFO is defined
   on ptx4 but it is not a signal.  Prevent process.c from doing the
   wrong thing.  */
#define BROKEN_SIGINFO

/* pae@dim.com (Phil Ernhardt) says this correction to
   the definition in usg5-4.h is needed to prevent
   all asynchronous subprocesses from exiting right away.  */

/* James Youngman <jay@gnu.org> found that on "DYNIX/ptx ARNIE 4.0
 * V4.4.2 i386", the push of the ttcompat module would fail.  It seems
 * that PTX 4.4.2 doesn't have that module, so if the push fails we
 * don't kill ourselves.  While this version lacks ttcompat, it also
 * has ptem, but the manual page for ptem indicates that it should
 * be pushed onto the slave side before the line discipline module.
 * See also the streampty manual page, if you're curious (and have
 * a ptx system).
 */

/* rms: I hope that older versions which do have ttcompat
   will not get confused by the code to use ptem.  */

#undef SETUP_SLAVE_PTY
#define SETUP_SLAVE_PTY \
  if (ioctl (xforkin, I_PUSH, "ptem") == -1 && errno != EINVAL) \
    fatal ("ioctl I_PUSH ptem", errno);     \
  if (ioctl (xforkin, I_PUSH, "ldterm") == -1)	\
    fatal ("ioctl I_PUSH ldterm", errno);	\
  if (ioctl (xforkin, I_PUSH, "ttcompat") == -1 && errno != EINVAL) \
    fatal ("ioctl I_PUSH ttcompat", errno);

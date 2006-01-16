#include "irix5-0.h"

/* as of version 5.2, irix no longer uses flock,
   according to jackr@wpd.sgi.com.  */
#undef MAIL_USE_FLOCK

/* C-g in select is not handled properly with restartable
   system calls.  So don't use them.  */
#undef SA_RESTART

/* schoepf@goofy.zdv.Uni-Mainz.de reports he needed -lw with X11R6
   on Irix 5.3.  I don't know which Irix version that need starts with.  */
#define NEED_LIBW

/* Looking at the wtmp file in filelock.c causes a crash
   for jpff@maths.bath.ac.uk.
   Note that irix6-0.h does not include this file, only irix5-0.h.  */
#define NO_WTMP_FILE

/* arch-tag: e2f820c1-2a3e-4cee-b5f1-6ce8ab21f439
   (do not change this comment) */

#include "irix5-0.h"

/* as of version 5.2, irix no longer uses flock, 
   according to jackr@wpd.sgi.com.  */
#undef MAIL_USE_FLOCK 

/* C-g in select is not handled properly with restartable
   system calls.  So don't use them.  */
#undef SA_RESTART

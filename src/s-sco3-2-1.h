/* Support SCO V 3.2.1 (also called Open Desk Top 1.0) */
/* For this to work,
   you must also comment out the include of termios.h in sysdep.c.
   Also, in time.el, change
	  (let ((process-connection-type nil))
   to....
	  (let ((process-connection-type t))  */

#include "s-sco3-2-2.h"	   

#undef SIGTSTP 	/* make suspend-emacs spawn a sub-shell */

#ifdef HAVE_X11
#define bzero(a,s)   memset(a,0,s) 
#define bcmp         memcmp
#define bcopy(a,b,s) memcpy(b,a,s)
#endif /* HAVE_X11 */

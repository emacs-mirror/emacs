/* s/ file for System V release 4.3.  */

#include "usg5-4-2.h"

/* Bill_Mann@PraxisInt.com: without this switch emacs generates this error
   on start up for an i486-ncr-sysv4.3 (running the X toolkit):
  _XipOpenIM() Unable to find Atom _XIM_INPUTMETHOD  */
#define X11R5_INHIBIT_I18N

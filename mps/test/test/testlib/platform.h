/* $Id$
   load appropriate header files to do platform-specific
   stuff.
*/

#ifdef MPS_OS_W3
#include "mpswin.h"
/* to trap access violations in the test harness */
LONG mySEHFilter(LPEXCEPTION_POINTERS);
#endif


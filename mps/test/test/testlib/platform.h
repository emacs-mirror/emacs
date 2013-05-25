/* $Id$
   load appropriate header files to do platform-specific
   stuff.
*/

#ifdef MPS_OS_W3
#ifdef MMQA_HEADER_mpsw3
/* we may be required to include mpsw3.h on windows platforms */
#include "mpsw3.h"
#endif
/* to trap access violations in the test harness */
LONG mySEHFilter(LPEXCEPTION_POINTERS);
#endif



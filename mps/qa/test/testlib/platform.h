/* $HopeName: MMQA_harness!testlib:platform.h(trunk.6) $
   load appropriate header files to do platform-specific
   stuff.
*/

#ifdef MPS_OS_SU

/* SunOS (4) doesn't have memmove, which would be handy for
   writing copy functions in formats. So...
*/

/* (I copied ossu.h from the mps header files for
    thursday afternoon)
*/

#include "ossu.h"
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#define CLOCKS_PER_SEC 1000000
int scanf(const char *format, ...);
void bcopy(char* from, char* to, int bytes);
void *memmove(void *to, void *from, size_t bytes);
#endif

#ifdef MPS_OS_W3
#ifdef MMQA_HEADER_mpsw3
/* we may be required to include mpsw3.h on windows platforms */
#include "mpsw3.h"
#endif
/* to trap access violations in the test harness */
LONG mySEHFilter(LPEXCEPTION_POINTERS);
#endif



#define AIX4

#include "aix3-2-5.h"

/* AIX 4 does not have HFT any more.  */
#undef AIXHFT

/* string.h defines rindex as a macro, at least with native cc, so we
   lose declaring char * rindex without this.
   It is just a guess which versions of AIX need this definition.  */
#undef HAVE_STRING_H

/* Dave Love <d.love@dl.ac.uk> reported this as needed on AIX 4.1.
   It is just a guess which versions of AIX need this definition.  */
#define HAVE_WAIT_HEADER

/* Specify the type that the 3rd arg of `accept' points to.
   It is just a guess which versions of AIX need this definition.  */
#define SOCKLEN_TYPE int

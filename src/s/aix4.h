#define AIX4

#include "aix3-2-5.h"

/* AIX 4 does not have HFT any more.  */
#undef AIXHFT

/* Dave Love <d.love@dl.ac.uk> reported this as needed on AIX 4.1.
   It is just a guess which versions of AIX need this definition.  */
#define HAVE_WAIT_HEADER

/* Specify the type that the 3rd arg of `accept' points to.
   It is just a guess which versions of AIX need this definition.  */
#define SOCKLEN_TYPE int

/* arch-tag: b9471dfc-ccdc-4980-a8a1-80c7627ec6b2
   (do not change this comment) */

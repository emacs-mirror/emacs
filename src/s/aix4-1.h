#define AIX4_1 

#include "aix4.h"

/* olson@mcs.anl.gov says -li18n is needed by -lXm.  */
#undef LIB_MOTIF
#define LIB_MOTIF -lXm -li18n

#ifdef __GNUC__
#undef _NO_PROTO
#endif

/* Let's hope the X internationalization stuff is working in AIX 4.1.
   It was not working in 3.2.5.  */
#undef X11R5_INHIBIT_I18N

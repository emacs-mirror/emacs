#define AIX4_1 

#include "aix4.h"

#if 0 /* Tomotake FURUHATA <furuhata@trl.ibm.co.jp> says this is needed
	 in Mule, but we don't know why.  Anyway, it's not needed now.  */
#define SYSTEM_MALLOC
#endif

/* olson@mcs.anl.gov says -li18n is needed by -lXm.  */
#define LIB_MOTIF -lXm -li18n

/* Cancel definition from aix3-1.h.  */
#undef sigsetmask

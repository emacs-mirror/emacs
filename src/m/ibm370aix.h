/* m/ file for IBM 370 running AIX.  */

#include "ibmps2-aix.h"

#define AIX

/* these were defined in "ibmps2-aix.h" */
#undef INTEL386
#undef aix386

/* i370 is  big-endian */
#define BIG_ENDIAN 

#undef TEXT_START
#undef SEGMENT_MASK
#undef DATA_SECTION_ALIGNMENT

#define TEXT_START 0
#define SEGMENT_MASK 0
#define DATA_SECTION_ALIGNMENT 0x00001000

#undef LOAD_AVE_CVT
#undef LOAD_AVE_TYPE
/* Data type of load average, as read out of kmem.  */
#define LOAD_AVE_CVT(x) (int)(((double) (x)) * 100.0 / 1.0)
#define LOAD_AVE_TYPE double

#undef LIBS_MACHINE
#define LIBS_MACHINE 
#undef HAVE_VFORK

#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -xa

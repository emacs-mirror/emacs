#include "vms.h"
#define VMS5_5
#define VMS4_4

#undef NO_HYPHENS_IN_FILENAMES

/* The bug that SHARABLE_LIB_BUG fixes is gone in version 5.5 of VMS.
   And defining it causes lossage because sys_errlist has a different
   number of elements.  */
#undef SHARABLE_LIB_BUG

#include "s-vms.h"
#define VMS5_5
#define VMS4_4

/* The bug that SHAREABLE_LIB_BUG fixes is gone in version 5.5 of VMS.
   And defining it causes lossage because sys_errlist has a different
   number of elements.  */
#undef SHAREABLE_LIB_BUG

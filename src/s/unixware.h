#include "usg5-4-2.h"

#ifndef __GNUC__
#undef HAVE_ALLOCA
#define C_ALLOCA
#endif

#define	PENDING_OUTPUT_COUNT(FILE) ((FILE)->__ptr - (FILE)->__base)

#include "sunos4-1.h"

#if 0 /* jik@gza.com says this didn't work.  Too bad.
	 Can anyone find out why this loses?  */
/* The bug that corrupts GNU malloc's memory pool is fixed in SunOS 4.1.3. */

#undef SYSTEM_MALLOC
#endif

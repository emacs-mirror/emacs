#define AIX4_1 

#include "aix4.h"

/* olson@mcs.anl.gov says -li18n is needed by -lXm.  */
#undef LIB_MOTIF
#define LIB_MOTIF -lXm -li18n

#ifdef __GNUC__
#undef _NO_PROTO
#endif

/* For AIX, it turns out compiling emacs under AIX 3.2.4 REQUIRES "cc -g"
   because "cc -O" crashes.  Under AIX 3.2.5, "cc -O" is required because
   "cc -g" crashes. Go figure.  --floppy@merlin.mit.edu.
   4.1 seems to need -g again. -- larry@vaquita.mitra.com.  */
#ifndef __GNUC__
#undef C_DEBUG_SWITCH
#undef C_OPTIMIZE_SWITCH
#define C_DEBUG_SWITCH -g
#endif

/* X internationalization stuff is still not working in AIX 4.1.  */
/* #undef X11R5_INHIBIT_I18N */

/* Perry Smith <pedz@ddivt1.austin.ibm.com> says this is necessary
   because of noncontiguous allocation.  */
#undef REL_ALLOC

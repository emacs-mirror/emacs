/* s/ file for BSDI BSD/OS 2.1 system.  */

#include "bsdos2.h"

#undef LIB_X11_LIB
#define LIB_X11_LIB -L/usr/X11/lib -lX11 -lipc

/* arch-tag: cf1ada4a-cdbf-452b-a264-ff84dd523e97
   (do not change this comment) */

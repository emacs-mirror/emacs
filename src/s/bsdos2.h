/* s/ file for BSDI BSD/OS 2.0 system.  */

#include "bsd386.h"

#define	TEXT_START	0x1020	/* for QMAGIC */
#define	START_FILES	pre-crt0.o /usr/lib/crt0.o
#define	HAVE_TERMIOS
#define	NO_TERMIO

#undef	KERNEL_FILE
#define	KERNEL_FILE	"/bsd"

/* arch-tag: 25ce1827-1511-4305-9058-24dd2118b5b4
   (do not change this comment) */

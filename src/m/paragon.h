/* m/ file for Paragon i860 machine.  */

#include "i860.h"
#define COFF
#define SYSTEM_MALLOC
#define TEXT_START 0x10000
#define LIB_STANDARD -lc -lic -lmach
#define KEEP_OLD_TEXT_SCNPTR
#define KEEP_OLD_PADDR
#define drem fmod

/* arch-tag: c1bc280c-25e5-4993-9b91-333c52ab3674
   (do not change this comment) */

/* m/ file for Paragon i860 machine.  */

#include "i860.h"
#define COFF
#define SYSTEM_MALLOC
#define TEXT_START 0x10000
#define LIB_STANDARD -lc -lic -lmach
#define KEEP_OLD_TEXT_SCNPTR
#define KEEP_OLD_PADDR
#define drem fmod

/* m/ file for Paragon i860 machine.  */

#include "i860.h"
#define COFF
#define SYSTEM_MALLOC
#define TEXT_START 0x10000
#define LD_DATA_START 0x140000
#undef  LD_SWITCH_SYSTEM
#define LD_SWITCH_SYSTEM -d LD_DATA_START
#define LIB_STANDARD -lc -lic
#define KEEP_OLD_TEXT_SCNPTR
#define KEEP_OLD_PADDR
#define drem fmod

/* versind.h
   lead appropriate header files to do version-specific
   interface things

   valid versions are:
    MO : modern -- i.e. as in thursday afternoon
    OS : oldstyle -- dylan.incr.patch.11
    GR : grotesque -- dylan.honeybee (space -> arena)
*/
#if defined(MMQA_VERS_GR)

#include "grotesq.h"

#elif defined(MMQA_VERS_MO)

#include "modern.h"

#elif defined(MMQA_VERS_OS)

#include "oldstyle.h"

#endif



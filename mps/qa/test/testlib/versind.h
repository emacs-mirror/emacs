/* $HopeName: MMQA_harness!testlib:versind.h(trunk.4) $
versind.h
   lead appropriate header files to do version-specific
   interface things

   valid versions are:
    MO : modern -- i.e. as in thursday afternoon
    OS : oldstyle -- dylan.incr.patch.11
    GR : grotesque -- dylan.honeybee (space -> arena)
    BQ : baroque -- dylan.meadowlark (have to include mpsw3.h)
    HU : humanist -- dylan.kinglet (spare committed fund)

  preXX.h makes new tests run with MMs before XX
  postXX.h makes old tests run with MMs from XX onwards

  Each postXX.h begins by including the previous one.
  Each preXX.h ends by including the next one.
*/

#if defined(MMQA_VERS_OS)
#include "preMO.h"

#elif defined(MMQA_VERS_MO)
#include "preGR.h"
#include "postMO.h"

#elif defined(MMQA_VERS_GR)
#include "preBQ.h"
#include "postGR.h"

#elif defined(MMQA_VERS_BQ)
#include "preHU.h"
#include "postBQ.h"

#else
#include "postHU.h"

#endif



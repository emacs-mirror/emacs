/* $HopeName: MMQA_harness!testlib:epvmfmt.h(trunk.4) $
epvmfmt.h
   Format for objects in the EPVM pool.
*/

#ifndef epvmfmt_h
#define epvmfmt_h

#include "testlib.h"

extern int alloccomments;

/* the counters are visible so that I can check whether things
   get moved etc
*/

/* the object format is visible so tests that want to
   can hack around with it
*/


typedef struct psobj psobj;

struct psobj {
 psobj *obj;
 mps_word_t size;
};

extern struct mps_fmt_A_s fmtepvm;

psobj *allocepvm(mps_ap_t ap, int words);

mps_res_t allocrepvm(psobj **, mps_ap_t ap, int words);

void splatepvm(psobj *obj);

mps_bool_t issplatepvm(psobj *obj);

#endif


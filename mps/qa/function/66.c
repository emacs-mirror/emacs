/* 
TEST_HEADER
 id = $HopeName$
 summary = test of location dependencies
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscmv.h"
#include "mpscamc.h"
#include "rankfmt.h"

#define MAXLDS 1000

void *stackpointer;

mps_space_t space;
static mycell *obj_table[MAXLDS];
static mps_ld_t lds[MAXLDS];

static void checklds(void) {
 int i;

 for (i=0; i < MAXLDS; i++) {
  if (obj_table[i]->data.copycount != 0) {
   asserts(mps_ld_isstale(lds[i], space, (mps_addr_t) obj_table[i]),
    "%d isn't stale but should be", i);
   if (ranint(4) == 0) {
    obj_table[i]->data.copycount = 0;
    mps_ld_reset(lds[i], space);
    comment("reset %d", i);
    mps_ld_add(lds[i], space, (mps_addr_t) obj_table[i]);
   }
  }
 }
}

static void test(void) {
 mps_pool_t poolmv, poolawl, poolamc;
 mps_thr_t thread;
 mps_root_t root0, root1, root2;

 mps_addr_t p;
 mps_fmt_t format;
 mps_ap_t apawl, apamc;

 mycell *a, *b;

 int i,j;

 RC;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root0, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_root_create_table(&root2, space, MPS_RANK_EXACT, 0,
                        (mps_addr_t *)obj_table, MAXLDS),
  "create table root");

 cdie(
  mps_root_create_reg(&root1, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create register and stack root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create awl pool");

 cdie(
  mps_pool_create(&poolmv, space, mps_class_mv(), 0x4000, 128, 0x4000),
  "create mv pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create amc pool");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 /* first we'll use only pool classes MV and AWL. So LDs shouldn't
    go stale at all.
 */

 b = allocone(apawl, 5, MPS_RANK_EXACT);

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  mps_alloc(&p, poolmv, sizeof(mps_ld_s));
  a = allocone(apawl, 5, MPS_RANK_EXACT);
  setref(a, 0, b);
  b = a;
  a = allocdumb(apamc, 256*1024, MPS_RANK_EXACT);
  a = allocdumb(apawl, 256*1024, MPS_RANK_EXACT);
  comment("alloc");
  lds[i] = p;
  mps_ld_reset(lds[i], space);
  comment("reset");
  if (i>0) {
   mps_ld_add(lds[i], space, (mps_addr_t) b);
  }
  comment("add");
 }

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  asserts(mps_ld_isstale(lds[i], space, p) == 0,
          "%d stale but shouldn't be", i);
 }

/* allocate MAXLDS objects, and make each LD depend on the corresponding
   object
*/

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  obj_table[i] = allocone(apamc, ranint(100), MPS_RANK_EXACT);
  mps_ld_add(lds[i], space, (mps_addr_t) obj_table[i]);
 }

 for (i=0; i < 1000; i++) {
  comment("%d of 1000", i);
  checklds();
  for (j=0; j < 4; j++) {
   a = allocdumb(apamc, 32*1024, MPS_RANK_EXACT);
  }
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolmv);
 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 mps_root_destroy(root2);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_space_destroy(space);
 comment("Destroyed space.");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

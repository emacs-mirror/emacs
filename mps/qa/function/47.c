/* 
TEST_HEADER
 id = $HopeName$
 summary = LDs don't go stale when using only non-moving pools
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscmv.h"
#include "mpscamc.h"
#include "rankfmt.h"

#define MAXLDS 100

void *stackpointer;

static void test(void) {
 mps_space_t space;
 mps_pool_t poolmv, poolawl;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_addr_t p;
 mps_ld_t lds[MAXLDS];
 mps_fmt_t format;
 mps_ap_t apawl;

 mycell *a, *b;

 int i;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root0, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

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

 b = allocone(apawl, 5, MPS_RANK_EXACT);

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  mps_alloc(&p, poolmv, sizeof(mps_ld_s));
  a = allocone(apawl, 5, MPS_RANK_EXACT);
  setref(a, 0, b);
  b = a;
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



 mps_ap_destroy(apawl);
 comment("Destroyed ap.");

 mps_pool_destroy(poolmv);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root0);
 mps_root_destroy(root1);
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

/* 
TEST_HEADER
 id = $Id$
 summary = LDs don't go stale when using only non-moving pools
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscmvff.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define MAXLDS 100

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t poolmvff, poolawl;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_addr_t p;
 mps_ld_t lds[MAXLDS];
 mps_fmt_t format;
 mps_ap_t apawl;

 mycell *a, *b;

 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root0, arena, mps_rank_ambig(), 0,
                            (mps_addr_t *)&exfmt_root, 1),
      "create exfmt root");

 cdie(mps_root_create_thread(&root1, arena, thread, stack_pointer), "thread root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
      "create awl pool");

 cdie(mps_pool_create_k(&poolmvff, arena, mps_class_mvff(), mps_args_none),
      "create MVFF pool");

 cdie(mps_ap_create(&apawl, poolawl, mps_rank_exact()),
      "create ap");

 b = allocone(apawl, 5, mps_rank_exact());

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  mps_alloc(&p, poolmvff, sizeof(mps_ld_s));
  a = allocone(apawl, 5, mps_rank_exact());
  setref(a, 0, b);
  b = a;
  a = allocdumb(apawl, 256*1024, mps_rank_exact());
  comment("alloc");
  lds[i] = p;
  mps_ld_reset(lds[i], arena);
  comment("reset");
  if (i>0) {
   mps_ld_add(lds[i], arena, (mps_addr_t) b);
  }
  comment("add");
 }

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  asserts(mps_ld_isstale(lds[i], arena, p) == 0,
          "%d stale but shouldn't be", i);
 }



 mps_arena_park(arena);
 mps_ap_destroy(apawl);
 comment("Destroyed ap.");

 mps_pool_destroy(poolmvff);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

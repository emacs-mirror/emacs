/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!126.c(trunk.1) $
 summary = extensible arena test (huge objects)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *b;

 /* create an arena that can't grow beyond 1 Mb */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*1)),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, MPS_RANK_AMBIG, 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0),
      "create root");

 cdie(mps_root_create_table(&root1, arena, MPS_RANK_AMBIG, 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_pool_create(&pool, arena, mps_class_amc(), format),
      "create pool");

 cdie(mps_ap_create(&ap, pool, MPS_RANK_EXACT),
      "create ap");

 comment("ready");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 b = allocdumb(ap, 1024ul*1024ul*40, MPS_RANK_EXACT);
 comment("alloc 40 MB");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 b = allocdumb(ap, 1024ul*1024ul*40, MPS_RANK_EXACT);
 comment("alloc 80 MB");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 mps_arena_collect(arena);
 comment("collected");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

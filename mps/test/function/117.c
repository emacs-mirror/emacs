/* 
TEST_HEADER
 id = $Id$
 summary = should collect objects on buffered segs (request.dylan.160064)
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 drop12 > 10000000
 drop13 > 10000000
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;

mps_arena_t arena;


static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *a, *b;

 size_t x, y, z;

 int i;

 /* create an arena that can't grow beyond 128 M */
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*128)),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                         mps_stack_scan_ambig, stackpointer, 0),
     "create root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 /* allocate a jolly big chain of objects */

 b = allocone(ap, 4, mps_rank_exact());

 for (i = 0; i < 256*1024; i++) {
  a = allocone(ap, 4, mps_rank_exact());
  setref(a, 0, b);
  b = a;
 }

 comment("%d objs allocated.", i);

 mps_arena_collect(arena);
 x = mps_arena_committed(arena);
 report("livesize", "%d", x);

 /* now let everything die, by destroying the only root and mps_arena_collect */

 mps_root_destroy(root);
 mps_arena_collect(arena);
 y = mps_arena_committed(arena);
 report("rootless", "%d", y);

 mps_ap_destroy(ap);
 mps_arena_collect(arena);
 z = mps_arena_committed(arena);
 report("apless", "%d", z);

 report("drop12", "%d", x - y);
 report("drop13", "%d", x - z);
 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_thread_dereg(thread);
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

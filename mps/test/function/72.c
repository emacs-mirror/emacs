/* 
TEST_HEADER
 id = $Id$
 summary = request.dylan.170439 (detect bad pointers)
 language = c
 link = testlib.o exfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= trace.c
 assertline = 963
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;
mycell *z;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 a = allocone(ap, 1024, 1);
 z = a;

 b = allocone(ap, 1024, 1);
 setref(b, 0, a);

 a = allocdumb(ap, 1024*64, 1);
 a = allocdumb(ap, 1024*64, 1);

 comment("Collecting...");
 mps_arena_collect(arena);
 asserts(z != a, "Didn't move!");

 comment("Writing bad pointer...");

 b->data.ref[0].addr = z;
 mps_arena_collect(arena);
 comment("Bad pointer not spotted in collection");

 fail();
 
 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

 mps_root_destroy(root);
 comment("Destroyed root.");

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

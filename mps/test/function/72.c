/* 
TEST_HEADER
 id = $Id$
 summary = request.dylan.170439 (detect bad pointers)
 language = c
 link = testlib.o exfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= trace.c
 assertcond = ss->rank < RankEXACT
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a[3], *bad;
 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 /* Clamp the arena so that we can be sure that objects don't move. */
 mps_arena_clamp(arena);

 cdie(mps_root_create_area(&root, arena, mps_rank_exact(), 0, &a[0], &a[3],
                           mps_scan_area, NULL), "create area");

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

 /* Remember the first allocation at a[0] (which is a root) and also
  * at bad (which is not reachable). */
 bad = a[0] = a[1] = allocone(ap, 1, 1);
 for (i = 0; i < 1000; ++i) {
  a[2] = allocone(ap, 1, 1);
  setref(a[2], 0, a[1]);
  a[1] = a[2];
 }

 /* The first collection will cause a[0] to move, but because bad
  * isn't scanned it doesn't get updated, and ends up pointing to
  * oldspace. */
 comment("Collecting...");
 mps_arena_collect(arena);
 asserts(bad != a[0], "Didn't move!");

 /* Write the bad pointer into a scannable part of the heap. The MPS
  * should spot this when it collects. Note that we can't use setref
  * here because we need to bypass the check. */
 comment("Writing bad pointer...");
 a[0]->data.ref[0].addr = bad;
 mps_arena_collect(arena);
 comment("Bad pointer not spotted in collection");

 fail();
 
 mps_arena_park(arena);
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

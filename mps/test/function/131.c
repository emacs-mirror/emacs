/* 
TEST_HEADER
 id = $Id$
 summary = allocation shouldn't fail if there's garbage to collect (2)
 language = c
 link = testlib.o rankfmt.o
 harness = 2.0
OUTPUT_SPEC
 predie = COMMIT_LIMIT
 postdie = OK
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


mps_pool_t poolmv;
mps_arena_t arena;


static void test(void)
{
 mps_pool_t pool;
 mps_root_t root;
 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap, ap2;
 mycell *a[2];
 mps_res_t res;
 int i;

 /* create an arena that can't grow beyond 1 M */
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*4)),
  "create arena");
 mps_arena_commit_limit_set(arena, (size_t) (1024*1024*1));

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 /* allocate until full */

 i = 0;
 a[0] = a[1] = NULL;
 cdie(mps_root_create_table(&root, arena, mps_rank_ambig(), 0, (void *)&a, 2),
      "create root");

 while (allocrone(&a[0], ap, 128, mps_rank_exact()) == MPS_RES_OK) {
  i++;
  setref(a[0], 0, a[1]);
  a[1] = a[0];
 }

 comment("%d objs allocated.", i);
 report("committed", "%ld", mps_arena_committed(arena));

 /* try to allocate 10 times */

 cdie(mps_ap_create(&ap2, pool, mps_rank_exact()), "create second ap");
 mps_ap_destroy(ap);

 for (i = 0; i < 10; i++) {
  res = allocrone(&a[0], ap2, 128, mps_rank_exact());
  report("predie", "%s", err_text(res));
 }

 /* now let everything die, and try to allocate 10 times */
 
 mps_root_destroy(root);

 for (i = 0; i < 10; i++) {
  res = allocrone(&a[0], ap2, 128, mps_rank_exact());
  report("postdie", "%s", err_text(res));
 }

 mps_arena_park(arena);
 mps_ap_destroy(ap2);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

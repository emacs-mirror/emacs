/* 
TEST_HEADER
 id = $Id$
 summary = AWL performance
 language = c
 link = testlib.o fastfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "mpsclo.h"
#include "fastfmt.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1,arena,mps_rank_ambig(),0,&exfmt_root,1),
  "create table root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_awl(), format, chain),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, arena, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolawl, mps_rank_exact()),
  "create ap");

 b = allocone(apamc, 1, mps_rank_exact());

 for (j=1; j<100; j++)
 {
  comment("%i of 100.", j);
  a = allocone(apamc, 5, mps_rank_exact());
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;

  for (i=1; i<5000; i++)
  {
   c = allocone(apamc, 20, mps_rank_exact());
   d = allocone(apawl, 20, mps_rank_exact());
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(b, 0, c);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

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

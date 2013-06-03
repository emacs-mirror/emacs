/* 
TEST_HEADER
 id = $Id$
 summary =  provoke segsummary assertion (request.dylan.170450)
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "awlfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *z[10];

 mycell **a = &z[0];
 mycell **b = &z[1];
 mycell **c = &z[2];
 mycell **d = &z[3];
 mycell **e = &z[4];
 mycell **f = &z[5];
 mycell **g = &z[6];

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_table(&root, arena, mps_rank_exact(), 0, (mps_addr_t*)&z[0], 10),
  "create table root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 for (j=1; j<100; j++)
 {
  comment("%i of 100.", j);
  UC;
  *a = allocone(ap, 5, 1);
  *b = *a;
  *c = *a;
  *d = *a;
  *e = *a;
  *f = *a;
  *g = *a;

  for (i=1; i<1000; i++)
  {
  UC;
   *c = allocone(ap, 1000, 1);
   if (ranint(8) == 0) *d = *c;
   if (ranint(8) == 0) *e = *c;
   if (ranint(8) == 0) *f = *c;
   if (ranint(8) == 0) *g = *c;
  UC;
   setref(*b, 0, *c);
  UC;
   setref(*c, 1, *d);
  UC;
   setref(*c, 2, *e);
  UC;
   setref(*c, 3, *f);
  UC;
   setref(*c, 4, *g);
  UC;
   *b = *c;
  }
 checkfrom(*a);
 }
 DC;
 DMC;

 checkfrom(*a);

 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
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

/* 
TEST_HEADER
 id = $Id$
 summary = mps_arena_park and mps_amc_apply
 language = c
 link = testlib.o exfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

#define MAGICSIZE ((size_t) 10342)

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;
long int appcount;
long int apppadcount;

static void test_apply(mps_addr_t addr, void *V, size_t S) {
 mycell *a;

 asserts(S == MAGICSIZE, "Size didn't get passed!");
 a = addr;
 asserts(((a->tag) & 0x3) == MCdata || ((a->tag) & 0x3) == MCpad,
         "apply on non-data and non-pad object at %p", addr);
 if (((a->tag) & 0x3) == MCdata) {
  appcount += 1;
 } else {
  apppadcount +=1;
 }
}

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t apamc;

 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1, arena, mps_rank_ambig(), 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++) {
  comment("%i of 10.", j);
  a = allocone(apamc, 5, 1);
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;
  for (i=1; i<1000; i++) {
   c = allocone(apamc, 1000, 1);
   if (ranint(8) == 0) d = c;
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(c, 0, b);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
  comment("parking...");
  mps_arena_park(arena);
  for (i=1; i<1000; i++) {
   c = allocone(apamc, 1000, 1);
   if (ranint(8) == 0) d = c;
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(c, 0, b);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
  a = c;
  b = c;
  d = c;
  e = c;
  f = c;
  g = c;
  mps_arena_collect(arena);
  comment("calling amc_apply:");
  checkfrom(c);
  appcount = 0;
  apppadcount = 0;
  mps_amc_apply(poolamc, &test_apply, NULL, MAGICSIZE);
  comment("finished amc_apply.");
  report("appcount", "%ld", appcount);
  report("apppadcount", "%ld", apppadcount);
  mps_arena_release(arena);
  comment("released.");
  RC;
 }

 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
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

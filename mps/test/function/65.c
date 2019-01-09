/* 
TEST_HEADER
 id = $Id$
 summary = sort-of-leak in arena_collect
 language = c
 link = testlib.o exfmt.o
OUTPUT_SPEC
 junksize = 0
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "exfmt.h"

#define MAGICSIZE ((size_t) 10342)

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


long int appcount;
long int apppadcount;
long int cutoff_id;
size_t junk_size;


static void test_apply(mps_addr_t addr, void *V, size_t S)
{
 mycell *a;
 long int id;

 asserts(S == MAGICSIZE, "Size didn't get passed!");
 a = addr;
 asserts(((a->tag) & 0x3) == MCdata || ((a->tag) & 0x3) == MCpad,
         "apply on non-data and non-pad object at %p", addr);
 if (((a->tag) & 0x3) == MCdata) {
  appcount += 1;
  id = getid(a);
  if (id < cutoff_id) {
   junk_size += a->data.size;
  }
 } else {
  apppadcount +=1;
 }
}


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root2, root3, root4, root5, root6, root7, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apamc;

 typedef mycell * myroot;
 myroot a, b, c, d, e, f, g;

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_table(&root1, arena, mps_rank_exact(), 0, (mps_addr_t*)&a, 1),
  "root");
 cdie(
  mps_root_create_table(&root2, arena, mps_rank_exact(), 0, (mps_addr_t*)&b, 1),
  "root");
 cdie(
  mps_root_create_table(&root3, arena, mps_rank_exact(), 0, (mps_addr_t*)&c, 1),
  "root");
 cdie(
  mps_root_create_table(&root4, arena, mps_rank_exact(), 0, (mps_addr_t*)&d, 1),
  "root");
 cdie(
  mps_root_create_table(&root5, arena, mps_rank_exact(), 0, (mps_addr_t*)&e, 1),
  "root");
 cdie(
  mps_root_create_table(&root6, arena, mps_rank_exact(), 0, (mps_addr_t*)&f, 1),
  "root");
 cdie(
  mps_root_create_table(&root7, arena, mps_rank_exact(), 0, (mps_addr_t*)&g, 1),
  "root");

 cdie(
  mps_root_create_table(&root, arena, mps_rank_exact(), 0, &exfmt_root, 1),
  "create exfmt root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 comment("parking...");
 mps_arena_park(arena);
 
 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++) {
  comment("%i of 10.", j);
  a = allocone(apamc, 5, 1);
  cutoff_id = getid(a);
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
  exfmt_root = c;
  mps_arena_collect(arena);
  comment(" reserved: %lu", (unsigned long) mps_arena_reserved(arena));
  comment("committed: %lu", (unsigned long) mps_arena_committed(arena));
  comment("calling amc_apply:");
  checkfrom(c);
  appcount = 0;
  apppadcount = 0;
  junk_size = 0;
  mps_amc_apply(poolamc, &test_apply, NULL, MAGICSIZE);
  comment("finished amc_apply");
  report("junksize", "%lu", (unsigned long) junk_size);
  report("appcount", "%ld", appcount);
  report("apppadcount", "%ld", apppadcount);
  RC;
 }

 mps_arena_release(arena);
 comment("released.");

 mps_arena_park(arena);
 mps_ap_destroy(apamc);
 mps_pool_destroy(poolamc);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);

 mps_root_destroy(root);
 mps_root_destroy(root1);
 mps_root_destroy(root2);
 mps_root_destroy(root3);
 mps_root_destroy(root4);
 mps_root_destroy(root5);
 mps_root_destroy(root6);
 mps_root_destroy(root7);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

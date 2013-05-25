/* 
TEST_HEADER
 id = $Id$
 summary = test of ramp allocation with smallish arena (64MB)
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define ARENALIMIT (64)

#define TABSIZE (50000)
#define ENTERRAMP (30000)
#define LEAVERAMP (100000)

#define BACKSIZE (128)
#define BACKITER (32)
#define RAMPSIZE (128)

#define ITERATIONS (1000000ul)

#define RAMP_INTERFACE
/*
#define COLLECT_WORLD
*/

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

mps_arena_t arena;
mps_pool_t poolamc;
mps_thr_t thread;
mps_root_t root, root1;

 mps_chain_t chain;
mps_fmt_t format;
mps_ap_t apamc;

static mps_addr_t objtab[TABSIZE];

static void alloc_back(void) {
 long int i, j;

 for (j = 0; j < BACKITER; j++) {
  i = ranint(ranint(ranint(ranint(TABSIZE)+1)+1)+1);
  objtab[i] = allocdumb(apamc, BACKSIZE, mps_rank_exact());
 }
}


static void test(void) {
 long int i;
 long int rsize;

 int inramp;

 mycell *r, *s;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1, arena, mps_rank_exact(), 0, &objtab[0], TABSIZE),
  "create root table");

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

 inramp = 0;

 for (i = 0; i < ITERATIONS; i++) {
  if (i % 10000 == 0) {
   comment("%ld of %ld", i, ITERATIONS);
  }
  alloc_back();
  if (inramp) {
   s = allocone(apamc, 3, mps_rank_exact());
   setref(r, 0, s);
   setref(s, 1, r);
   r = s;
   s = allocdumb(apamc, RAMPSIZE, mps_rank_exact());
   setref(r, 2, s);
   rsize ++;
   if (ranint(LEAVERAMP) == 0) {
    r = allocone(apamc, 2, mps_rank_exact());
    s = allocone(apamc, 2, mps_rank_exact());
#ifdef RAMP_INTERFACE
    mps_ap_alloc_pattern_end(apamc, mps_alloc_pattern_ramp());
#endif
#ifdef COLLECT_WORLD
    mps_arena_collect(arena);
    mps_arena_release(arena);
#endif
    comment("ramp end, %ld objects", rsize);
    inramp = 0;
   }
  } else {
   if (ranint(ENTERRAMP) == 0) {
#ifdef RAMP_INTERFACE
    mps_ap_alloc_pattern_begin(apamc, mps_alloc_pattern_ramp());
#endif
    comment("ramp begin");
    r = allocone(apamc, 3, mps_rank_exact());
    inramp = 1;
    rsize = 0;
   }
  }
 }

 mps_ap_destroy(apamc);
 comment("Destroyed ap.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

 mps_root_destroy(root1);
 mps_root_destroy(root);
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
 report("result", "pass");
 return 0;
}

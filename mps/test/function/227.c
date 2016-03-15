/* 
TEST_HEADER
 id = $Id$
 summary = allocate in 2 arenas
 language = c
 link = testlib.o rankfmt.o
 parameters = ITERATIONS=10000
OUTPUT_SPEC
 result = pass
END_HEADER
*/

/* This test is basically MMQA_test_function!124.c modified to
   used two arenas
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define ARENALIMIT (100)

#define TABSIZE (ITERATIONS / 2)
#define ENTERRAMP (ITERATIONS / 10)
#define LEAVERAMP (ITERATIONS / 10)

#define BACKSIZE (32)
#define BACKITER (32)
#define RAMPSIZE (128)

#define RAMP_INTERFACE
/*
#define COLLECT_WORLD
*/

void *stackpointer;

mps_arena_t arena1, arena2;
mps_pool_t poolamc1, poolamc2;
mps_thr_t thread1, thread2;
mps_root_t root1, root1a, root2, root2a;

mps_fmt_t format1, format2;
mps_ap_t apamc1, apamc2;

static mps_addr_t objtab1[TABSIZE], objtab2[TABSIZE];

static void alloc_back(void) {
 long int i, j;
 for (j = 0; j < BACKITER; j++) {
  i = ranint(ranint(ranint(ranint(TABSIZE)+1)+1)+1);
  objtab1[i] = allocdumb(apamc1, BACKSIZE*2, mps_rank_exact());
  i = ranint(ranint(ranint(ranint(TABSIZE)+1)+1)+1);
  objtab2[i] = allocdumb(apamc2, BACKSIZE, mps_rank_exact());
 }
}

static void test(void) {
 long int i;
 long int rsize;

 int inramp;

 mycell *r1, *r2, *s1, *s2;

 cdie(mps_arena_create(&arena1, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT), "create arena");
 cdie(mps_arena_create(&arena2, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT), "create arena");

 cdie(mps_thread_reg(&thread1, arena1), "register thread");
 cdie(mps_thread_reg(&thread2, arena2), "register thread");

 cdie(
  mps_root_create_reg(&root1, arena1, mps_rank_ambig(), 0, thread1,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");
 cdie(
  mps_root_create_reg(&root2, arena2, mps_rank_ambig(), 0, thread2,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1a, arena1, mps_rank_exact(), 0, &objtab1[0], TABSIZE),
  "create root table");
 cdie(
  mps_root_create_table(&root2a, arena2, mps_rank_exact(), 0, &objtab2[0], TABSIZE),
  "create root table");

 cdie(
  mps_fmt_create_A(&format1, arena1, &fmtA),
  "create format");
 cdie(
  mps_fmt_create_A(&format2, arena2, &fmtA),
  "create format");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format1);
   cdie(mps_pool_create_k(&poolamc1, arena1, mps_class_amc(), args),
        "create pool");
 } MPS_ARGS_END(args);
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format2);
   cdie(mps_pool_create_k(&poolamc2, arena2, mps_class_amc(), args),
        "create pool");
 } MPS_ARGS_END(args);

 cdie(
  mps_ap_create(&apamc1, poolamc1, mps_rank_exact()),
  "create ap");
 cdie(
  mps_ap_create(&apamc2, poolamc2, mps_rank_exact()),
  "create ap");

 inramp = 0;

 for (i = 0; i < ITERATIONS; i++) {
  if (i * 10 % ITERATIONS == 0) {
   comment("%ld of %ld", i, ITERATIONS);
  }
  alloc_back();
  if (inramp) {
   s1 = allocone(apamc1, 3, mps_rank_exact());
   s2 = allocone(apamc2, 3, mps_rank_exact());
   setref(r1, 0, s1);
   setref(r2, 0, s2);
   setref(s1, 1, r1);
   setref(s2, 1, r2);
   r1 = s1;
   r2 = s2;
   s1 = allocdumb(apamc1, RAMPSIZE, mps_rank_exact());
   s2 = allocdumb(apamc2, RAMPSIZE, mps_rank_exact());
   setref(r1, 2, s1);
   setref(r2, 2, s2);
   rsize ++;
   if (ranint(LEAVERAMP) == 0) {
    r1 = allocone(apamc1, 2, mps_rank_exact());
    r2 = allocone(apamc2, 2, mps_rank_exact());
    s1 = allocone(apamc1, 2, mps_rank_exact());
    s2 = allocone(apamc2, 2, mps_rank_exact());
#ifdef RAMP_INTERFACE
    mps_ap_alloc_pattern_end(apamc1, mps_alloc_pattern_ramp());
    mps_ap_alloc_pattern_end(apamc2, mps_alloc_pattern_ramp());
#endif
#ifdef COLLECT_WORLD
    mps_arena_collect(arena1);
    mps_arena_collect(arena2);
    mps_arena_release(arena1);
    mps_arena_release(arena2);
#endif
    comment("ramp end, %ld objects", rsize);
    inramp = 0;
   }
  } else {
   if (ranint(ENTERRAMP) == 0) {
#ifdef RAMP_INTERFACE
    mps_ap_alloc_pattern_begin(apamc1, mps_alloc_pattern_ramp());
    mps_ap_alloc_pattern_begin(apamc2, mps_alloc_pattern_ramp());
#endif
    comment("ramp begin");
    r1 = allocone(apamc1, 3, mps_rank_exact());
    r2 = allocone(apamc2, 3, mps_rank_exact());
    inramp = 1;
    rsize = 0;
   }
  }
 }

 mps_arena_park(arena1);
 mps_arena_park(arena2);
 mps_ap_destroy(apamc1);
 mps_ap_destroy(apamc2);
 comment("Destroyed ap.");

 mps_pool_destroy(poolamc1);
 mps_pool_destroy(poolamc2);
 comment("Destroyed pool.");

 mps_fmt_destroy(format1);
 mps_fmt_destroy(format2);
 comment("Destroyed format.");

 mps_root_destroy(root1);
 mps_root_destroy(root1a);
 mps_root_destroy(root2);
 mps_root_destroy(root2a);
 comment("Destroyed roots.");

 mps_thread_dereg(thread1);
 mps_thread_dereg(thread2);
 comment("Deregistered thread.");

 mps_arena_destroy(arena1);
 mps_arena_destroy(arena2);
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

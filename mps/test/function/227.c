/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!124.c(trunk.2) $
 summary = allocate in 2 arenas
 language = c
 link = testlib.o rankfmt.o
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

#define TABSIZE (50000)
#define ENTERRAMP (30000)
#define LEAVERAMP (100000)

#define BACKSIZE (32)
#define BACKITER (32)
#define RAMPSIZE (128)

#define ITERATIONS (1000000ul)

#define RAMP_INTERFACE
/*
#define COLLECT_WORLD
*/

void *stackpointer;

mps_space_t arena1, arena2;
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
  objtab1[i] = allocdumb(apamc1, BACKSIZE*2, MPS_RANK_EXACT);
  i = ranint(ranint(ranint(ranint(TABSIZE)+1)+1)+1);
  objtab2[i] = allocdumb(apamc2, BACKSIZE, MPS_RANK_EXACT);
 }
}

static void test(void) {
 long int i;
 long int rsize;

 int inramp;

 mycell *r1, *r2, *s1, *s2;

 cdie(mps_arena_create(&arena1, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT), "create space");
 cdie(mps_arena_create(&arena2, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT), "create space");

 cdie(mps_thread_reg(&thread1, arena1), "register thread");
 cdie(mps_thread_reg(&thread2, arena2), "register thread");

 cdie(
  mps_root_create_reg(&root1, arena1, MPS_RANK_AMBIG, 0, thread1,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");
 cdie(
  mps_root_create_reg(&root2, arena2, MPS_RANK_AMBIG, 0, thread2,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1a, arena1, MPS_RANK_EXACT, 0, &objtab1[0], TABSIZE),
  "create root table");
 cdie(
  mps_root_create_table(&root2a, arena2, MPS_RANK_EXACT, 0, &objtab2[0], TABSIZE),
  "create root table");

 cdie(
  mps_fmt_create_A(&format1, arena1, &fmtA),
  "create format");
 cdie(
  mps_fmt_create_A(&format2, arena2, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc1, arena1, mps_class_amc(), format1),
  "create pool");
 cdie(
  mps_pool_create(&poolamc2, arena2, mps_class_amc(), format2),
  "create pool");

 cdie(
  mps_ap_create(&apamc1, poolamc1, MPS_RANK_EXACT),
  "create ap");
 cdie(
  mps_ap_create(&apamc2, poolamc2, MPS_RANK_EXACT),
  "create ap");

 inramp = 0;

 for (i = 0; i < ITERATIONS; i++) {
  if (i % 10000 == 0) {
   comment("%ld of %ld", i, ITERATIONS);
  }
  alloc_back();
  if (inramp) {
   s1 = allocone(apamc1, 3, MPS_RANK_EXACT);
   s2 = allocone(apamc2, 3, MPS_RANK_EXACT);
   setref(r1, 0, s1);
   setref(r2, 0, s2);
   setref(s1, 1, r1);
   setref(s2, 1, r2);
   r1 = s1;
   r2 = s2;
   s1 = allocdumb(apamc1, RAMPSIZE, MPS_RANK_EXACT);
   s2 = allocdumb(apamc2, RAMPSIZE, MPS_RANK_EXACT);
   setref(r1, 2, s1);
   setref(r2, 2, s2);
   rsize ++;
   if (ranint(LEAVERAMP) == 0) {
    r1 = allocone(apamc1, 2, MPS_RANK_EXACT);
    r2 = allocone(apamc2, 2, MPS_RANK_EXACT);
    s1 = allocone(apamc1, 2, MPS_RANK_EXACT);
    s2 = allocone(apamc2, 2, MPS_RANK_EXACT);
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
    r1 = allocone(apamc1, 3, MPS_RANK_EXACT);
    r2 = allocone(apamc2, 3, MPS_RANK_EXACT);
    inramp = 1;
    rsize = 0;
   }
  }
 }

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
 comment("Destroyed space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 report("result", "pass");
 return 0;
}

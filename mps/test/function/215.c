/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!215.c(trunk.2) $
 summary = test of ramp allocation
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

#define ARENALIMIT (200)

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

void *stackpointer;

mps_space_t arena;
mps_pool_t poolamc;
mps_thr_t thread;
mps_root_t root, root1;

mps_fmt_t format;
mps_ap_t apamc;

static mps_addr_t objtab[TABSIZE];

static void alloc_back(void) {
 long int i, j;

 for (j = 0; j < BACKITER; j++) {
  i = ranint(ranint(ranint(ranint(TABSIZE)+1)+1)+1);
  objtab[i] = allocdumb(apamc, BACKSIZE, MPS_RANK_EXACT);
 }
}


static void test(void) {
 long int i;
 long int rsize;
 mps_message_t message;

 int inramp;

 mycell *r, *s;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
   (size_t) 1024*1024*ARENALIMIT),
  "create space");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1, arena, MPS_RANK_EXACT, 0, &objtab[0], TABSIZE),
  "create root table");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 mps_message_type_enable(arena, mps_message_type_gc());

 inramp = 0;

 for (i = 0; i < ITERATIONS; i++) {
  if (i % 10000 == 0) {
   comment("%ld of %ld", i, ITERATIONS);
  }
  alloc_back();
  if (inramp) {
   s = allocone(apamc, 3, MPS_RANK_EXACT);
   setref(r, 0, s);
   setref(s, 1, r);
   r = s;
   s = allocdumb(apamc, RAMPSIZE, MPS_RANK_EXACT);
   setref(r, 2, s);
   rsize ++;
   if (ranint(LEAVERAMP) == 0) {
    r = allocone(apamc, 2, MPS_RANK_EXACT);
    s = allocone(apamc, 2, MPS_RANK_EXACT);
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
    r = allocone(apamc, 3, MPS_RANK_EXACT);
    inramp = 1;
    rsize = 0;
   }
  }
  if(mps_message_get(&message, arena, mps_message_type_gc())) {
    unsigned long live, condemned, notCondemned;
    live = mps_message_gc_live_size(arena, message);
    condemned = mps_message_gc_condemned_size(arena, message);
    notCondemned = 
      mps_message_gc_not_condemned_size(arena, message);
    comment("Collection: live=%ld,  condemned=%ld,  not condemned = %ld",
      live, condemned, notCondemned);
    mps_message_discard(arena, message);
  }
 }

 mps_ap_destroy(apamc);
 comment("Destroyed ap.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root1);
 mps_root_destroy(root);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
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

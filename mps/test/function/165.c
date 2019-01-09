/* 
TEST_HEADER
 id = $Id$
 summary = simple spare commit limit test
 language = c
 link = testlib.o rankfmt.o
 harness = 2.0
 parameters = EXTEND=65536 AVGSIZE=32 BIGSIZE=5000000;
OUTPUT_SPEC
 spare_committed0 = 0
 spare0 = 0.000000
 reduce1 > 4000000
 spare_committed1 = 0
 spare1 = 0.000000
 reduce2 <= 0
 spare_committed2 > 4000000
 spare2 = 1.000000
 reduce3 > 3000000
 spare_committed3 < 50000
 spare3 = 0.010000
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

mps_arena_t arena;

#define MAXOBJS (10000)

mps_addr_t objs[MAXOBJS];
mps_addr_t sizes[MAXOBJS];

static void test(void *stack_pointer)
{
 mps_pool_t pool;
 mps_thr_t thread;

 size_t com0, com1, com2;

/* create a VM arena of 40MB with commit limit of 100MB, i.e. let the
   arena do the limiting. */

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1024*1024*40);
   MPS_ARGS_ADD(args, MPS_KEY_COMMIT_LIMIT, 1024ul*1024ul*100ul);
   MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0.0);
   cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "create arena");
 } MPS_ARGS_END(args);
 report("spare_committed0", "%lu", (unsigned long)mps_arena_spare_committed(arena));
 report("spare0", "%f", mps_arena_spare(arena));

 cdie(mps_thread_reg(&thread, arena), "register thread");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, EXTEND);
   MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, AVGSIZE);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, 0);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, 0);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, 1);
   /* Set SPARE to 0 as we are testing arena hysteresis here and we
      don't want MVFF hysteresis to get in the way. */
   MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0.0);
   cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), args),
        "create low pool");
 } MPS_ARGS_END(args);

 die(mps_alloc(&objs[0], pool, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_free(pool, objs[0], BIGSIZE);
 com1 = mps_arena_committed(arena);

/* the free should have reduced the total amount committed */
 report("reduce1", "%ld", com0-com1);
 report("spare_committed1", "%lu", (unsigned long)mps_arena_spare_committed(arena));
 report("spare1", "%f", mps_arena_spare(arena));

/* Try again but with arena hysteresis */
 mps_arena_spare_set(arena, 1.0);
 asserts(mps_arena_spare(arena) <= 1.0, "spare");
 die(mps_alloc(&objs[0], pool, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_free(pool, objs[0], BIGSIZE);
 com1 = mps_arena_committed(arena);

/* This time the free shouldn't make any difference */
 report("reduce2", "%ld", com0-com1);
 report("spare_committed2", "%lu", (unsigned long)mps_arena_spare_committed(arena));
 report("spare2", "%f", mps_arena_spare(arena));

/* Reducing the spare committed limit should return most of the spare */
 mps_arena_spare_set(arena, 0.01);
 com2 = mps_arena_committed(arena);
 report("reduce3", "%ld", com0-com2);
 report("spare_committed3", "%lu", (unsigned long)mps_arena_spare_committed(arena));
 report("spare3", "%f", mps_arena_spare(arena));

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

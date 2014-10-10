/* 
TEST_HEADER
 id = $Id$
 summary = simple spare_commit_limit test
 language = c
 link = testlib.o rankfmt.o
 harness = 2.0
 parameters = EXTEND=65536 AVGSIZE=32 BIGSIZE=5000000;
OUTPUT_SPEC
 reduce1 > 4000000
 reduce2 <= 0
 reduce3 > 3000000
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

static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;

 unsigned long com0, com1, com2;

/* create a VM arena of 40MB */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*1024*40)),
  "create arena");

/* set the commit limit to 100MB, i.e. let the arena do the limiting */

 mps_arena_commit_limit_set(arena, (size_t) (1024ul*1024ul*100ul));

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

/* Set the spare commit limit to 0MB */

 mps_arena_spare_commit_limit_set(arena, (size_t) 0);
 die(mps_alloc(&objs[0], pool, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_free(pool, objs[0], BIGSIZE);
 com1 = mps_arena_committed(arena);

/* the free should have reduced the total amount committed */
 report("reduce1", "%ld", com0-com1);

/* Try again but with arena hysteresis */

/* nb. size_t unsigned, therefore (size_t)-1 is the maximum limit */
 mps_arena_spare_commit_limit_set(arena, (size_t)-1);
 die(mps_alloc(&objs[0], pool, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_free(pool, objs[0], BIGSIZE);
 com1 = mps_arena_committed(arena);

/* This time the free shouldn't make any difference */
 report("reduce2", "%ld", com0-com1);

/* Reducing the spare committed limit should return most of the spare */
 mps_arena_spare_commit_limit_set(arena, (size_t)(1024*1024));
 com2 = mps_arena_committed(arena);
 report("reduce3", "%ld", com0-com2);

 comment("Finishing off.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}


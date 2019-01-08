/* 
TEST_HEADER
 id = $Id$
 summary = simple spare commit limit test
 language = c
 link = testlib.o rankfmt.o
 harness = 2.0
 parameters = EXTEND=65536 AVGSIZE=32 BIGSIZE=5000000;
OUTPUT_SPEC
 reduce1 <= 0
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
 mps_pool_t poolhi, poollo;
 mps_thr_t thread;

 size_t com0, com1;

/* create a VM arena of 40MB with commit limit of 100MB, i.e. let the
   arena do the limiting. */

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1024*1024*40);
   MPS_ARGS_ADD(args, MPS_KEY_COMMIT_LIMIT, 1024ul*1024ul*100ul);
   MPS_ARGS_ADD(args, MPS_KEY_SPARE, 1.0);
   cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "create arena");
 } MPS_ARGS_END(args);

 cdie(mps_thread_reg(&thread, arena), "register thread");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, EXTEND);
   MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, AVGSIZE);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, 1);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, 1);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, 0);
   cdie(mps_pool_create_k(&poolhi, arena, mps_class_mvff(), args),
        "create high pool");
 } MPS_ARGS_END(args);

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, EXTEND);
   MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, AVGSIZE);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, 0);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, 0);
   MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, 1);
   /* Set SPARE to 0 as we want this pool to return memory to the
      arena as soon as it is freed so we can allocate it elsewhere. */
   MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0.0);
   cdie(mps_pool_create_k(&poollo, arena, mps_class_mvff(), args),
        "create low pool");
 } MPS_ARGS_END(args);

/* allocate a jolly big object, clamp the commit limit down, leaving
   128KB space, then free it */

 die(mps_alloc(&objs[0], poollo, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_arena_commit_limit_set(arena, com0+(1024*128));

 mps_free(poollo, objs[0], BIGSIZE);
 com1 = mps_arena_committed(arena);

/* the free shouldn't have reduced the total amount committed */
 report("reduce1", "%ld", com0-com1);

/* it should be possible to reallocate the object */
 die(mps_alloc(&objs[0], poollo, BIGSIZE), "alloc lo");

 mps_free(poollo, objs[0], BIGSIZE);

/* it should equally be possible to allocate an object in a different
 segment (poolhi). This ought to flush the spare. */
 die(mps_alloc(&objs[0], poolhi, BIGSIZE), "alloc hi");

 mps_free(poolhi, objs[0], BIGSIZE);

 comment("Finishing off.");

 mps_pool_destroy(poolhi);
 mps_pool_destroy(poollo);
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


/* 
TEST_HEADER
 id = $Id$
 summary = simple spare_commit_limit test
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

#define MVFF_HI_PARMS EXTEND,AVGSIZE,MPS_PF_ALIGN,1,1,0
#define MVFF_LO_PARMS EXTEND,AVGSIZE,MPS_PF_ALIGN,0,0,1

mps_arena_t arena;

#define MAXOBJS (10000)

mps_addr_t objs[MAXOBJS];
mps_addr_t sizes[MAXOBJS];

static void test(void)
{
 mps_pool_t poolhi, poollo;
 mps_thr_t thread;

 unsigned long com0, com1;

/* create a VM arena of 40MB */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*1024*40)),
  "create arena");


/* set the commit limit to 100MB, i.e. let the arena do the limiting */

 mps_arena_commit_limit_set(arena, (size_t) (1024ul*1024ul*100ul));

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(&poolhi, arena, mps_class_mvff(), MVFF_HI_PARMS),
  "create high pool");

 cdie(
  mps_pool_create(&poollo, arena, mps_class_mvff(), MVFF_LO_PARMS),
  "create low pool");

/* set the spare commit limit to something very big */
 mps_arena_spare_commit_limit_set(arena, (size_t)-1);

/* allocate a jolly big object, clamp the commit limit down, leaving
   64KB space, then free it */

 die(mps_alloc(&objs[0], poollo, BIGSIZE), "alloc");
 com0 = mps_arena_committed(arena);
 mps_arena_commit_limit_set(arena, com0+(1024*64));

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


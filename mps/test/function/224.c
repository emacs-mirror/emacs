/* 
TEST_HEADER
 id = $Id$
 summary = MV allocate large promise, make it small, repeat
 language = c
 link = testlib.o
 harness = 2.5
 parameters = EXTENDBY=65536 AVGSIZE=32 PROMISE=64 ITERATE=2000
OUTPUT_SPEC
 errtext = alloc: COMMIT_LIMIT
END_HEADER

This one is supposed to fail, telling us that MV is badly fragmented.
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpsavm.h"


#define VMSIZE ((size_t) 30*1024*1024)


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), VMSIZE), "create");
 die(mps_arena_commit_limit_set(arena, VMSIZE), "commit limit");

 die(mps_pool_create(&pool, arena, mps_class_mv(),
                     EXTENDBY, AVGSIZE, EXTENDBY),
     "pool create");

 for (p=0; p<ITERATE; p++) {
  die(mps_alloc(&q, pool, PROMISE*1024), "alloc");
  q = (mps_addr_t) ((char *) q + 8);
  mps_free(pool, q, PROMISE*1024-8);
  report("promise", "%i", p);
 }

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}


int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = MVFF allocate large promise, make it small, repeat
 language = c
 link = testlib.o
 harness = 2.5
 parameters = PROMISE=65536 ITERATE=2000
END_HEADER
*/

#include "mpsavm.h"
#include "mpscmvff.h"
#include "testlib.h"


#define VMSIZE ((size_t) 30*1024*1024)


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), VMSIZE), "create");
 die(mps_arena_commit_limit_set(arena, VMSIZE), "commit limit");
 die(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none),
     "pool create");

 for (p=0; p<ITERATE; p++) {
  die(mps_alloc(&q, pool, PROMISE), "alloc");
  q = (char *)q + MPS_PF_ALIGN;
  mps_free(pool, q, PROMISE - MPS_PF_ALIGN);
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

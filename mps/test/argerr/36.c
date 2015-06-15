/* 
TEST_HEADER
 id = $Id$
 summary = wrong size_t to free (MV)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= dbgpool.c
 assertcond = tag->size == size
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "arg.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;

 mps_addr_t a,b;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mv_debug(), mps_args_none),
      "create pool");

 die(mps_alloc(&a, pool, 8),
     "alloc a");
 die(mps_alloc(&b, pool, 32),
     "alloc b");
 mps_free(pool, a, 9);
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

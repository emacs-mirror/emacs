/* 
TEST_HEADER
 id = $Id$
 summary = MVFF fenceposting check: subfree
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= dbgpool.c
 assertcond = fencepost check on free
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

mps_arena_t arena;

static mps_pool_debug_option_s debugOpts = {(void *)"bibblebo", 8};

static void test(void *stack_pointer)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_addr_t a;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
  (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(mps_pool_create(&pool, arena, mps_class_mvff_debug(), &debugOpts,
                     (size_t)8192, (size_t)8, (mps_align_t)8,
                     (mps_bool_t)1, (mps_bool_t)0, (mps_bool_t)0),
     "create MVFF pool");

 die(mps_alloc(&a, pool, 64), "alloc a");
 
 mps_free(pool, a, 32);

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void) {
 run_test(test);
 pass();
 return 0;
}

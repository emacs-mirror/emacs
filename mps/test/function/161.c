/* 
TEST_HEADER
 id = $Id$
 summary = MVFF debug fenceposting check: subfree
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


void *stackpointer;
mps_arena_t arena;

static mps_pool_debug_option_s debugOpts = {(void *)"bibblebo", 8};


static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_addr_t a;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
  (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debugOpts);
   die(mps_pool_create_k(&pool, arena, mps_class_mvff_debug(), args),
       "create MVFF pool");
 } MPS_ARGS_END(args);

 die(mps_alloc(&a, pool, 64), "alloc a");
 
 mps_free(pool, a, 32);

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

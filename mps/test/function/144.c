/* 
TEST_HEADER
 id = $Id$
 summary = MVFF fenceposting check
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertcond = fencepost check requested by client
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

void *stackpointer;
mps_arena_t arena;

static mps_pool_debug_option_s debugOpts = {(void *)"bibblebo", 8};

static void test(void) {
 mps_thr_t thread;
 mps_pool_t pool;
 mps_addr_t a, b;
 char *c;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
  (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(
  mps_pool_create(&pool, arena, mps_class_mvff_debug(), &debugOpts,
                  8192, 8, 8, 0, 0, 1),
  "create MVFF pool");

 die(mps_alloc(&a, pool, 64), "alloc a");
 die(mps_alloc(&b, pool, 64), "alloc b");
 
 c = a;
 c -= 1;
 *c = 0;
 
 mps_pool_check_fenceposts(pool);
 comment("Fencepost check.");

 c += 67;
 *c = 0;

 mps_pool_check_fenceposts(pool);
 comment("Fencepost check.");

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void) {
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = very small arenas
 language = c
 link = testlib.o
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpscmv.h"

void *stackpointer;

mps_arena_t arena;
mps_thr_t thread;
mps_pool_t pool;
mps_pool_t pools[100];

static void test(void) {

 int i;
 for (i=64; i>0; i--) {
  comment("Trying arena of %dKB.", i);
  cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*i)),
   "create arena");
  cdie(mps_thread_reg(&thread, arena), "register thread");
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
 }
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

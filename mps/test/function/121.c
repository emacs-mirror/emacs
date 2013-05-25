/* 
TEST_HEADER
 id = $Id$
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


static void test(void)
{
  int i;
  for (i = 64; i >= 0; i--) {
    mps_res_t res;

    comment("Trying arena of %d kB.", i);
    res = mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*i));
    if (res == MPS_RES_OK) {
      res = mps_thread_reg(&thread, arena);
      if (res == MPS_RES_OK) {
        mps_thread_dereg(thread);
      } else {
        if (res != MPS_RES_MEMORY) {
          error("Wrong error code, %d, for mps_thread_reg.", res);
        }
      }
      mps_arena_destroy(arena);
    } else {
      report_res("arena_create", res);
      if (res != MPS_RES_MEMORY) {
        error("Wrong error code.");
      }
    }
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

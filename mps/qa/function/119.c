/* $HopeName: MMQA_test_function!117.c(trunk.1) $
TEST_HEADER
 summary = create a VM arena with non-aligned size
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

void *stackpointer;

mps_arena_t arena;

static void test(void)
{

/* create an arena that can't grow beyond 128 M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*10+1)),
  "create arena");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

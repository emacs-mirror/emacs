/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena which contains a thread
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = RingIsSingle(&arena->threadRing)
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

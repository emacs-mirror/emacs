/* 
TEST_HEADER
 id = $Id$
 summary = register thread twice
 language = c
 link = myfmt.o testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_thr_t thread2;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_thread_reg(&thread2, arena), "register thread 2");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_thread_dereg(thread2);
 comment("Deregistered thread 2.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}


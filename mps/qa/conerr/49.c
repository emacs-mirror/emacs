/* 
TEST_HEADER
 id = $HopeName$
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
 mps_space_t space;
 mps_thr_t thread;
 mps_thr_t thread2;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_thread_reg(&thread2, space), "register thread 2");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_thread_dereg(thread2);
 comment("Deregistered thread 2.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}


/* test destroy a space which contains a thread
   language c
   link testlib.o
*/

#include "testlib.h"
#include "mpscmv.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_thr_t thread;

 cdie(mps_space_create(&space), "create space");
 
 cdie(mps_thread_reg(&thread, space), "register thread");

 mps_space_destroy(space);
 comment("Destroy space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

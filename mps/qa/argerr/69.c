/* 
TEST_HEADER
 id = $HopeName$
 summary = UNALIGNED thr_t to thread_reg
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(UNALIGNED, space), "register thread");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

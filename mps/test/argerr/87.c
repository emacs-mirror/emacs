/* 
TEST_HEADER
 id = $HopeName$
 summary = UNALIGNED space for is_stale
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
 mps_ld_s ld;
 mps_thr_t thread;
 mps_addr_t p;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 mps_ld_reset(&ld, space);

 mps_ld_add(&ld, space, &p);

 mps_ld_isstale(&ld, UNALIGNED, &p);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = < AMBIG rank for mps_root_create_reg (with stack scan ambig)
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
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_root_create_reg(&root, space, MPS_RANK_AMBIG-1, 0, 
                      thread, mps_stack_scan_ambig, stackpointer, 0),
      "root create");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

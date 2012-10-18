/* 
TEST_HEADER
 id = $HopeName$
 summary = UNALIGNED 1st arg to fmt_create_A
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "arg.h"
#include "newfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(UNALIGNED, space, &fmtA),
  "create format");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

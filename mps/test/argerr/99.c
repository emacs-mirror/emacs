/* 
TEST_HEADER
 id = $HopeName$
 summary = highbit set root mode for mps_root_create
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

void *stackpointer;

static mps_res_t rootscan(mps_ss_t ss, void *p, size_t s)
{
 return MPS_RES_OK;
}

static void test(void)
{
 mps_space_t space;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_root_create(&root, space, MPS_RANK_AMBIG, HIGHBIT_INT,
                      rootscan, NULL, 0),
      "root create");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

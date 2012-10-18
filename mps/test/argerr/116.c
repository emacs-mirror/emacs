/* 
TEST_HEADER
 id = $HopeName$
 summary = UNALIGNED &root_t for mps_root_create_fmt
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

void *stackpointer;

static mps_res_t fmtscan(mps_ss_t ss,
  mps_addr_t base, mps_addr_t limit)
{
 return MPS_RES_OK;
}

static void test(void)
{
 mps_space_t space;
 mps_thr_t thread;
 mps_addr_t a[32];

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_root_create_fmt(UNALIGNED, space, MPS_RANK_AMBIG, 0, 
                      fmtscan, a, &a[32]),
      "root create");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

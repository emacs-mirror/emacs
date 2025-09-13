/* 
TEST_HEADER
 id = $Id$
 summary = UNALIGNED arena for mps_root_create_fmt
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static mps_res_t fmtscan(mps_ss_t ss,
  mps_addr_t base, mps_addr_t limit)
{
 return MPS_RES_OK;
}

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_addr_t a[32];
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_fmt(&root, UNALIGNED, mps_rank_ambig(), 0, 
                      fmtscan, a, &a[32]),
      "root create");

}

int main(void)
{
 run_test(test);
 return 0;
}

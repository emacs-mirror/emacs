/* 
TEST_HEADER
 id = $Id$
 summary = -1 root mode for mps_root_create
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = mps_rm == (mps_rm_t)0
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static mps_res_t rootscan(mps_ss_t ss, void *p, size_t s)
{
 return MPS_RES_OK;
}

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create(&root, arena, mps_rank_ambig(), -1,
                      rootscan, NULL, 0),
      "root create");

}

int main(void)
{
 run_test(test);
 return 0;
}

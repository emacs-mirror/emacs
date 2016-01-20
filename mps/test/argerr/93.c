/* 
TEST_HEADER
 id = $Id$
 summary = UNALIGNED arena for mps_root_create
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
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
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create(&root, UNALIGNED, mps_rank_ambig(), 0,
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

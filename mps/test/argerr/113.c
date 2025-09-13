/* 
TEST_HEADER
 id = $Id$
 summary = unaligned size for mps_root_create_table
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;
 mps_addr_t a[30];

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root, arena,
        mps_rank_ambig(), 0, a, (size_t) 31),
      "root create");

}

int main(void)
{
 run_test(test);
 return 0;
}

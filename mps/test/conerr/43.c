/* 
TEST_HEADER
 id = $Id$
 summary = destroy root though uncreated
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_root_t root = (mps_root_t)1;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

/*
 cdie(
  mps_root_create_table(&root, arena, mps_rank_ambig(), 0,
   roottable, sizeof(mps_addr_t[10])), "create root");
*/

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}


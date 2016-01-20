/* 
TEST_HEADER
 id = $Id$
 summary = create root in destroyed arena
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_root_t root;
 mps_addr_t roottable[10];

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

 cdie(
  mps_root_create_table(&root, arena, mps_rank_ambig(), 0,
   roottable, sizeof(mps_addr_t[10])), "create root");

 mps_root_destroy(root);
 comment("Destroyed root.");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}


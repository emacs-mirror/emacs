/* 
TEST_HEADER
 id = $Id$
 summary = create an arena and then destroy it, twice!
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_arena_t arena;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "Create arena");
 mps_arena_destroy(arena);
 comment("Destroy arena once.");
 mps_arena_destroy(arena);
 comment("Destroy arena twice!");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

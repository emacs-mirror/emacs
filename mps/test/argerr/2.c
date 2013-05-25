/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena with an null arena_t
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void)
{
 mps_arena_t arena;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "Create arena");
 mps_arena_destroy(NULL);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

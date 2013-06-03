/* 
TEST_HEADER
 id = $Id$
 summary = add to ld in wrong arena
 language = c
 link = myfmt.o testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void)
{
 mps_arena_t arena;
 mps_arena_t arena1;
 mps_ld_s ld;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 cdie(mps_arena_create(&arena1), "create arena 1");

 mps_ld_reset(&ld, arena);
 comment("Reset ld."); 

 mps_ld_add(&ld, arena1, &arena);
 comment("Added to ld.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}


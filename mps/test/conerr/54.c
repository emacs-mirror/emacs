/* 
TEST_HEADER
 id = $Id$
 summary = is_stale without resetting
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= ld.c
 assertcond = ld->_epoch <= history->epoch
END_HEADER
*/

#include <string.h>

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_ld_s ld;

 /* overwrite ld with junk */
 memset(&ld, 0xff, sizeof ld);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

/*
 mps_ld_reset(&ld, arena);
 comment("Reset ld."); 
*/

/*
 mps_ld_add(&ld, arena, &arena);
 comment("Added to ld.");
*/

 report("isstale", "%d", mps_ld_isstale(&ld, arena, &arena));

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 run_test(test);
 return 0;
}


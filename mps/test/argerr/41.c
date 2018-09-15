/* 
TEST_HEADER
 id = $Id$
 summary = zero extendBy for pool_create (MVFF)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolmvff.c
 assertcond = extendBy > 0
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "arg.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, 0);
   cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), args), "pool");
 } MPS_ARGS_END(args);

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

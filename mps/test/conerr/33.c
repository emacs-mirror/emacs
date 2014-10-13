/* 
TEST_HEADER
 id = $Id$
 summary = create an AP with a rank not supported by its pool
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscams.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_fmt_t format;
 mps_ap_t ap;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   cdie(mps_pool_create_k(&pool, arena, mps_class_ams(), args),
        "create pool");
 } MPS_ARGS_END(args);

 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_weak());
   cdie(mps_ap_create_k(&ap, pool, args), "create ap");
 } MPS_ARGS_END(args);

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

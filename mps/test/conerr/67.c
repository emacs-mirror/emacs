/* 
TEST_HEADER
 id = $Id$
 summary = AMS pool asserts on exact reference to unallocated object
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolams.c
 assertcond = ss->rank == RankAMBIG
END_HEADER
*/

#include "testlib.h"
#include "mpscams.h"
#include "myfmt.h"

static void test(void)
{
 void *marker = &marker;
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;
 mps_fmt_t format;
 mps_ap_t ap;
 mps_addr_t p, q, unallocated;

 cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "create arena");
 mps_arena_park(arena);
 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, marker), "create root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   cdie(mps_pool_create_k(&pool, arena, mps_class_ams(), args), "pool");
 } MPS_ARGS_END(args);
 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "ap");

 /* p is in the AMS pool */
 p = allocone(ap, 0, NULL, NULL, sizeof(mycell));

 /* q is in the AMS pool with exact references to p and unallocated object */
 unallocated = (void *)((char*)p + 2 * sizeof(mycell));
 q = allocone(ap, 1, p, unallocated, sizeof(mycell));

 /* Destroy the allocation point so that the segment gets unbuffered. */
 mps_ap_destroy(ap);

 mps_arena_start_collect(arena);
 mps_arena_park(arena);

 /* Keep q (and thus p) alive during the collection. */
 report("q", "%p", q);

 mps_pool_destroy(pool);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

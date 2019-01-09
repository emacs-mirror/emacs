/* 
TEST_HEADER
 id = $Id$
 summary = AWL pool asserts on exact out-of-bounds reference
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolawl.c
 assertcond = ss->rank == RankAMBIG
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 void *marker = &marker;
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;
 mps_fmt_t fmt;
 mps_ap_t ap;
 mps_addr_t p, q, out_of_bounds;
 size_t header = sizeof(mycell);

 cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "create arena");
 mps_arena_park(arena);
 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, marker), "create root");
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, header);
   fmtargs(args + 1);
   cdie(mps_fmt_create_k(&fmt, arena, args), "format");
 } MPS_ARGS_END(args);
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
   cdie(mps_pool_create_k(&pool, arena, mps_class_awl(), args), "pool");
 } MPS_ARGS_END(args);
 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "ap");

 /* p is in the AWL pool */
 p = allocheader(ap, 0, NULL, NULL, sizeof(mycell), header);

 /* q is in the AMS pool with exact reference to p and out-of-bounds object */
 out_of_bounds = (void *)((char*)p - header);
 q = allocheader(ap, 1, p, out_of_bounds, sizeof(mycell), header);

 /* Destroy the allocation point so that the segment gets unbuffered. */
 mps_ap_destroy(ap);

 mps_arena_start_collect(arena);
 mps_arena_park(arena);

 /* Keep q (and thus p) alive during the collection. */
 report("q", "%p", q);

 mps_pool_destroy(pool);
 mps_fmt_destroy(fmt);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 return 0;
}

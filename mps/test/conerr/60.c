/* 
TEST_HEADER
 id = $Id$
 summary = LO pool asserts on unaligned exact reference
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poollo.c
 assertcond = ss->rank == RankAMBIG
END_HEADER
*/

#include "testlib.h"
#include "mpscams.h"
#include "mpsclo.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 void *marker = &marker;
 mps_arena_t arena;
 mps_pool_t pool_ams, pool_lo;
 mps_thr_t thread;
 mps_root_t root;
 mps_fmt_t format;
 mps_ap_t ap_ams, ap_lo;
 mps_addr_t p, q, unaligned;

 cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "create arena");
 mps_arena_park(arena);
 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, marker), "create root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   cdie(mps_pool_create_k(&pool_ams, arena, mps_class_ams(), args), "ams pool");
 } MPS_ARGS_END(args);
 MPS_ARGS_BEGIN(args) {
   MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
   cdie(mps_pool_create_k(&pool_lo, arena, mps_class_lo(), args), "lo pool");
 } MPS_ARGS_END(args);
 cdie(mps_ap_create(&ap_ams, pool_ams, mps_rank_exact()), "ams ap");
 cdie(mps_ap_create_k(&ap_lo, pool_lo, mps_args_none), "lo ap");

 /* p is in the LO pool */
 p = allocone(ap_lo, 0, NULL, NULL, sizeof(mycell));

 /* q is in the AMS pool with unaligned exact reference to p */
 unaligned = (void *)((char*)p + 1);
 q = allocone(ap_ams, 1, p, unaligned, sizeof(mycell));

 mps_arena_start_collect(arena);
 mps_arena_park(arena);

 /* Keep q (and thus p) alive during the collection. */
 report("q", "%p", q);

 mps_ap_destroy(ap_lo);
 mps_ap_destroy(ap_ams);
 mps_pool_destroy(pool_lo);
 mps_pool_destroy(pool_ams);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 return 0;
}

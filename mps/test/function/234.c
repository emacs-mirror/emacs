/* 
TEST_HEADER
 id = $Id$
 summary = empty traces (regression test for job004086)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscams.h"
#include "mpscawl.h"
#include "mpsclo.h"
#include "rankfmt.h"
#include "testlib.h"

static void test_pool(void *stack_pointer, mps_pool_class_t pool_class)
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_thr_t thread;
  mps_root_t root;
  mps_fmt_t format;
  mps_ap_t ap;
  void *addr;

  cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
       "create arena");
  cdie(mps_thread_reg(&thread, arena), "register thread");
  cdie(mps_root_create_thread(&root, arena, thread, stack_pointer),
       "create thread");
  cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
    cdie(mps_pool_create_k(&pool, arena, pool_class, args), "create pool");
  } MPS_ARGS_END(args);
  cdie(mps_ap_create_k(&ap, pool, mps_args_none), "create ap");

  /* First reserve on the allocation point causes the pool to create a
     buffered segment but with no objects yet. */
  cdie(mps_reserve(&addr, ap, MPS_PF_ALIGN), "reserve");

  /* Create a trace and condemn the world, but discover that no
     objects were condemned, so abort the trace. */
  mps_arena_collect(arena);

  /* Collect again to ensure that aborting the trace after condemning
     didn't violate the trace invariants. */
  mps_arena_collect(arena);

  asserts(mps_commit(ap, &addr, MPS_PF_ALIGN), "commit");

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  mps_root_destroy(root);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
}

static void test(void *stack_pointer)
{
  test_pool(stack_pointer, mps_class_amc());
  test_pool(stack_pointer, mps_class_ams());
  test_pool(stack_pointer, mps_class_awl());
  test_pool(stack_pointer, mps_class_lo());
}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

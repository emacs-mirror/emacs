/* 
TEST_HEADER
 id = $Id$
 summary = create/configure arena with too-small commit limit
 language = c
 link = testlib.o
OUTPUT_SPEC
 create1 = COMMIT_LIMIT
 create2 = OK
 configure = FAIL
END_HEADER
*/

#include "testlib.h"
#include "newfmt.h"

static void test(void)
{
  mps_arena_t arena;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_COMMIT_LIMIT, 16 * 1024);
    report_res("create1",
               mps_arena_create_k(&arena, mps_arena_class_vm(), args));
  } MPS_ARGS_END(args);

  report_res("create2", 
             mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none));

  report_res("configure", mps_arena_commit_limit_set(arena, 16 * 1024));

  mps_arena_destroy(arena);
}

int main(void)
{
  easy_tramp(test);
  pass();
  return 0;
}

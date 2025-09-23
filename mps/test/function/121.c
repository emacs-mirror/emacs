/* 
TEST_HEADER
 id = $Id$
 summary = very small arenas
 language = c
 link = testlib.o
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpsacl.h"

mps_arena_t arena;

static char buffer[1024 * 1024];

static void test(void *stack_pointer)
{
  mps_res_t res, prev_res = MPS_RES_OK;
  int i;
  
  /* VM arenas round up small sizes and so creation must succeed. */
  for (i = 1024; i >= 0; i -= i/17 + 1) {
    MPS_ARGS_BEGIN(args) {
      MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1024 * i);
      die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
          "mps_arena_create");
    } MPS_ARGS_END(args);
    mps_arena_destroy(arena);
  }

  /* Client arenas have to work within the memory they are given and
   * so must fail at some point. */
  for (i = 1024; i >= 0; i -= i/17 + 1) {
    MPS_ARGS_BEGIN(args) {
      MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_BASE, buffer);
      MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1024 * i);
      res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
    } MPS_ARGS_END(args);
    if (res == MPS_RES_OK) {
      if (prev_res != MPS_RES_OK) {
        error("Success with smaller size.");
      }
      mps_arena_destroy(arena);
    } else {
      if (res != MPS_RES_MEMORY) {
        report_res("arena_create", res);
        error("Wrong error code.");
      }
    }
    prev_res = res;
  }
  if (res != MPS_RES_MEMORY) {
    error("Wrong error code.");
  }
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

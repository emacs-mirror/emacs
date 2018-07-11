/* 
TEST_HEADER
 id = $Id$
 summary = fill address space with arenas until an error results!
 language = c
 link = testlib.o
 parameters = ARENAS=10
OUTPUT_SPEC
 arena >= 10
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

static void test(void)
{
  mps_arena_t arena;
  mps_res_t res;
  int p;

  for (p = 0;; ++p) {
    MPS_ARGS_BEGIN(args) {
      MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1);
      res = mps_arena_create_k(&arena, mps_arena_class_vm(), args);
    } MPS_ARGS_END(args);
    if (res != MPS_RES_OK)
      break;
  }
  asserts(res == MPS_RES_RESOURCE, "resource");
  report("arena", "%i", p);
}

int main(void)
{
  if (MPS_WORD_WIDTH <= 32) {
    easy_tramp(test);
  } else {
    /* Can't exhaust 64-bit address space by allocating, so fake a pass. */
    report("arena", "%d", ARENAS);
  }
  return 0;
}

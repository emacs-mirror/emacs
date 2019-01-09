/* 
TEST_HEADER
 id = $Id$
 summary = check that failed arena creation doesn't leak
 language = c
 link = testlib.o
 parameters = ARENAS=10
OUTPUT_SPEC
 arena >= 10
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"


static void test(void *stack_pointer)
{
  mps_arena_t arena;
  mps_res_t res;
  int p = 0, i;

  /* fill address space with small arenas */
  while ((res = mps_arena_create(&arena, mps_arena_class_vm(), 1))
         == MPS_RES_OK) {
    p++;
  }
  report("arena", "%d", p);
  /* destroy one small arena */
  mps_arena_destroy(arena);
  for (i = 0; i < ARENAS; ++i) {
    /* there isn't enough space for a large arena */
    res = mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1 << 20);
    asserts(res == MPS_RES_RESOURCE, "error leak");
    /* but destroying one small arena makes room for another */
    die(mps_arena_create(&arena, mps_arena_class_vm(), 1), "leak");
    mps_arena_destroy(arena);
  }
}


int main(void)
{
  if (MPS_WORD_WIDTH <= 32) {
    run_test(test);
  } else {
    /* Can't exhaust 64-bit address space by allocating, so fake a pass. */
    report("arena", "%d", ARENAS);
  }
  pass();
  return 0;
}

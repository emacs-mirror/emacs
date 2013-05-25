/* 
TEST_HEADER
 id = $Id$
 summary = create arenas until an error results, see if it leaks at failure
 language = c
 link = testlib.o
OUTPUT_SPEC
 arena > 10
 arena_tight = 129
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"


#define minArenaSIZE ((size_t)(130 * 1024))


static void test(void)
{
  mps_arena_t arena, previousArena = NULL;
  mps_res_t res;
  size_t size = (size_t)(1024*1024*10L);
  int p = 0, i;

  /* make sure you can create at least 10 */
  while ((res = mps_arena_create(&arena, mps_arena_class_vm(), size))
         == MPS_RES_OK) {
    p++;
    report("arena", "%i", p);
  }
  asserts(res == MPS_RES_RESOURCE, "wrong error loop");
  /* fill address space with arenas */
  while (size > 2 * minArenaSIZE) {
    size /= 2;
    res = mps_arena_create(&arena, mps_arena_class_vm(), size);
    asserts(res == MPS_RES_OK || res == MPS_RES_RESOURCE, "error fill");
    if (res == MPS_RES_OK) p++;
  }
  report("arena2", "%i", p);
  report("size", "%i", size);
  /* there could still be holes, fill some more */
  while ((res = mps_arena_create(&arena, mps_arena_class_vm(), minArenaSIZE))
         == MPS_RES_OK) {
    p++; previousArena = arena;
  }
  mps_arena_destroy(previousArena);
  report("arena3", "%i", p);
  /* test that you can create and fail without leaking */
  for (i = 0; i < minArenaSIZE / 1024; ++i) {
    res = mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*1024*10L));
    asserts(res == MPS_RES_RESOURCE, "error leak");
    die(mps_arena_create(&arena, mps_arena_class_vm(), minArenaSIZE), "leak");
    report("arena_tight", "%i", i);
    mps_arena_destroy(arena);
  }
}


int main(void)
{
  easy_tramp(test);
  return 0;
}

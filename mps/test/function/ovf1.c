/*
TEST_HEADER
 id = $Id$
 summary = tests allocating objects larger than INT_MAX
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

/*
  The function ProtSet in code/protix.c contained this assertion:

    AVER(AddrOffset(base, limit) <= INT_MAX).

  I changed that to:

     AVER(AddrOffset(base, limit) <= PTRDIFF_MAX)

  which makes more sense on typical 64bit systems.  This test exercises
  that code path.

  (Unfortunately, this test can trigger the AVER(error != ENOMEM)
   assertion in that function if the machine has not enough RAM.
   Strictly speaking, that is a separate bug/problem.)
*/

#include "ephfmt.h"
#include <stdio.h>
#include <string.h>

static void test(mmp mm, void* closure)
{
  unsigned i = 0;
  size_t N = 10;
  for (; i != N; i++) {
    oop o = alloc_bytes(mm->amc_ap, (size_t)INT_MAX);
    if (o == NULL)
      comment("alloc_bytes failed without crashing");
    mm->roots[i % 2] = o;
    mps_arena_start_collect(mm->arena);
    while (mps_arena_step(mm->arena, 0, 0))
      ;
  }
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}

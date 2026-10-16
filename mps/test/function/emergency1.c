/*
TEST_HEADER
 id = $Id$
 summary = tests emergency fixing with weak references
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

/* Currently, the AMC pool does not implement weak pointer semantics in
   emergency fixing.  This can trigger the .check.band.weak assertion in
   trace.c to fail.

   This test creates some weak references to the AMC pool and puts the
   arena in emergency mode to trigger that assertion.
 */

#include "ephfmt.h"
#include "mpm.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static void test(mmp mm, void* closure)
{
  size_t i, j;
  size_t u = ArenaGrainSize(mm->arena) * 5;
  die(mps_arena_commit_limit_set(mm->arena, 64 * u),
      "mps_arena_commit_limit_set");
  for (i = 0; i != 3; i++)
    mm->roots[i] = make_string(mm->amc_ap, 16 * u, NULL);
  mm->roots[4] = make_pair(mm->awl0_ap, mm->roots[0], NULL);
  for (i = 0; i != 3; i++)
    mm->roots[i] = NULL;
  for (j = 0; j != 10; j++)
    for (i = 0; i != 2; i++)
      mm->roots[i] = make_string(mm->amc_ap, 8 * u, NULL);
  asserts(mm->roots[4]->pair.car == NULL,
          "mm->roots[4]->pair.car should be NULL");
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}

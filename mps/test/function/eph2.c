/*
TEST_HEADER
 id = $Id$
 summary = test TraceRankForAccess for ephemeron pool
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

#include "ephfmt.h"
#include <stdio.h>
#include <string.h>

static void test(mmp mm, void* closure)
{
  int i, j;
  oop* s = mm->roots;
  memset(s, 0, sizeof(mm->roots));
  s[99] = string_from_cstr(mm->amc_ap, "x");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  s[2] = make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[2]);
  s[0] = NULL;
  s[1] = NULL;
  s[2] = NULL;

  /* The first mps_arena_step scans the AMC segment.  The second step
   * scans the AEPH segment.  The trace is finished after two steps, if
   * we access the AEPH segment before the third step; otherwise the
   * trace finishes after the third step.
   *
   * Starting the loop at j=0 triggers the "unreachable code" assertion
   * in trace.c:1047.
   */
  for (j = 1; j < 3; j++) {
    die(mps_arena_start_collect(mm->arena), "mps_arena_start_collect");
    for (i = 0; i < j; i++)
      cdie(mps_arena_step(mm->arena, 0, 0) == 0, "some work done");
    cdie(s[3]->weak_pair.key == NULL, "key reachable");
    cdie(s[3]->weak_pair.value == NULL, "value reachable");
    while (mps_arena_step(mm->arena, 0, 0))
      ;
    cdie(mps_arena_step(mm->arena, 0, 0) != 0, "no more work to do");
    cdie(s[3]->weak_pair.key == NULL, "key reachable");
    cdie(s[3]->weak_pair.value == NULL, "value reachable");
  }
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}

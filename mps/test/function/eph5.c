/*
TEST_HEADER
 id = $Id$
 summary = test TraceRankForAccess for AMC segments from ephemeron band
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

#include "ephfmt.h"
#include "mpm.h"
#include <stdio.h>
#include <string.h>

static void build_chain(mmp mm, size_t length, size_t tail_idx)
{
  size_t i;
  oop* loc = &mm->roots[tail_idx];
  for (i = 0; i < length; i++)
    *loc = make_pair(mm->amc_ap, NULL, *loc);
}

static void test(mmp mm, void* closure)
{
  oop* s = mm->roots;
  memset(s, 0, sizeof(mm->roots));
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = NULL;
  /* Build a chain that spans multiple segments in the AMC pool. */
  build_chain(mm, 32 * 1024 / sizeof(struct pair), 1);
  s[2] = make_weak_pair(mm->eph_ap, s[0], s[1]);
  s[0] = NULL;
  s[1] = NULL;

  die(mps_arena_start_collect(mm->arena), "mps_arena_start_collect");

  /* Advance the trace until we reach the ephemeron band. */
  {
    Arena arena = mm->arena;
    TraceStruct* t = &arena->trace[0];
    asserts(arena->busyTraces != TraceSetEMPTY, "trace busy");
    while (t->band != RankEPHEMERON && mps_arena_step(mm->arena, 0, 0))
      asserts(arena->busyTraces != TraceSetEMPTY, "trace busy");
    asserts(arena->busyTraces != TraceSetEMPTY, "trace busy");
    asserts(t->band == RankEPHEMERON, "in ephemeron band");
  }

  /* Touch all objects in the chain. */
  {
    oop chain = s[2]->weak_pair.value;
    while (chain && chain->pair.header.s.type == TYPE_PAIR)
      chain = chain->pair.cdr;
    asserts(mm->arena->busyTraces != TraceSetEMPTY, "trace busy");
    asserts(mm->arena->trace[0].band == RankEPHEMERON,
            "in ephemeron band");
  }

  while (mps_arena_step(mm->arena, 0, 0))
    ;
  asserts(mps_arena_step(mm->arena, 0, 0) == 0, "no more work to do");
  /* The weak pair is retained for longer than logically necessary
   * because we accessed it in the ephemeron band.  */
  asserts(s[0] == NULL, "s[0] is NULL");
  asserts(s[2]->weak_pair.key != NULL, "key reachable");
  asserts(s[2]->weak_pair.value != NULL, "value reachable");
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}

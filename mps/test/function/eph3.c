/*
TEST_HEADER
 id = $Id$
 summary = test mps_fix_weak_or_pair
 language = c
 link = testlib.o ephfmt.o
END_HEADER
*/

#include "ephfmt.h"
#include <stdio.h>
#include <string.h>

static void test_reachable_key(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_reachable_key");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_or_pair(mm->eph_ap, s[0], s[1]);
  s[1] = NULL;
  asserts(s[3]->weak_pair.key == s[0], "key == s[0]");
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key != NULL, "key reachable");
  asserts(s[3]->weak_pair.key == s[0], "key == s[0]");
  asserts(s[3]->weak_pair.value != NULL, "value reachable");
  check_string(s[3]->weak_pair.key, "a");
  check_string(s[3]->weak_pair.value, "b");
}

static void test_unreachable_key(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_unreachable_key");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_or_pair(mm->eph_ap, s[0], s[1]);
  s[0] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key != NULL, "key reachable");
  asserts(s[3]->weak_pair.value != NULL, "value reachable");
  asserts(s[3]->weak_pair.value == s[1], "key == s[1]");
  check_string(s[3]->weak_pair.key, "a");
  check_string(s[3]->weak_pair.value, "b");
}

static void test_both_unreachable(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_both_unreachable");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_or_pair(mm->eph_ap, s[0], s[1]);
  s[0] = NULL;
  s[1] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key == NULL, "key unreachable");
  asserts(s[3]->weak_pair.value == NULL, "value unreachable");
}

static void test_both_reachable(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_both_reachable");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_and_pair(mm->eph_ap, s[0], s[1]);
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key != NULL, "key reachable");
  asserts(s[3]->weak_pair.value != NULL, "value reachable");
  check_string(s[3]->weak_pair.key, "a");
  check_string(s[3]->weak_pair.value, "b");
  asserts(s[3]->weak_pair.key == s[0], "key == s[0]");
  asserts(s[3]->weak_pair.value == s[1], "value == s[1]");
}

static void test(mmp mm, void* closure)
{
  typedef void (*fun)(mmp, oop s[ROOT_COUNT]);
  fun funs[] = { test_reachable_key,
                 test_unreachable_key,
                 test_both_unreachable,
                 test_both_reachable };
  size_t i;
  for (i = 0; i < sizeof funs / sizeof funs[0]; i++) {
    memset(mm->roots, 0, sizeof mm->roots);
    funs[i](mm, mm->roots);
  }
}

int main(int argc, char* argv[])
{
  run_eph_test(test, NULL);
  pass();
  return 0;
}

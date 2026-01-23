/*
TEST_HEADER
 id = $Id$
 summary = tests for ephemerons
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
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[1]);
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
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[1]);
  s[0] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key == NULL, "key unreachable");
  asserts(s[3]->weak_pair.value == NULL, "value unreachable");
}

static void test_key_eq_value(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_key_eq_value");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  make_pair(mm->amc_ap, s[0], s[0]);
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[0]);
  s[0] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key == NULL, "key unreachable");
  asserts(s[3]->weak_pair.value == NULL, "value unreachable");
}

static void test_value_unreachable(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_value_unreachable");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  s[2] = make_pair(mm->amc_ap, s[0], s[1]);
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[2]);
  s[1] = NULL;
  s[2] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key != NULL, "key reachable");
  asserts(s[3]->weak_pair.value != NULL, "value reachable");
  check_string(s[3]->weak_pair.key, "a");
  asserts(s[3]->weak_pair.value->header.type == TYPE_PAIR,
          "val->header.type == TYPE_PAIR");
  asserts(s[3]->weak_pair.value->pair.car == s[0],
          "s[3]->weak_pair.value->pair.car == s[0]");
  check_string(s[3]->weak_pair.value->pair.cdr, "b");
}

static void test_chain(mmp mm, oop s[ROOT_COUNT])
{
  /* Create a ring where only one key is externally reachable.  All
   * other keys are only reachable through a value of a weak pair. */
  unsigned n = 5;
  unsigned i;
  comment("test_chain");
  for (i = 0; i < n; i++) {
    char keyname[10];
    char valname[10];
    sprintf(keyname, "key%u", i);
    sprintf(valname, "val%u", i);
    s[4 * i + 0] = string_from_cstr(mm->amc_ap, keyname);
    s[4 * i + 1] = string_from_cstr(mm->amc_ap, valname);
  }
  for (i = 0; i < n; i++) {
    s[4 * i + 2] =
      make_pair(mm->amc_ap, s[4 * i + 1], s[4 * ((i + 1) % n)]);
    s[4 * i + 3] = make_weak_pair(mm->eph_ap, s[4 * i], s[4 * i + 2]);
  }
  for (i = 0; i < n; i++) {
    if (i != (n - 1) / 2)
      s[4 * i + 0] = NULL;
    s[4 * i + 1] = NULL;
    s[4 * i + 2] = NULL;
  }
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  for (i = 0; i < n; i++) {
    oop eph = s[4 * i + 3];
    asserts(eph->weak_pair.key != NULL, "key reachable");
    asserts(eph->weak_pair.value != NULL, "value reachable");
    {
      char buf[10];
      sprintf(buf, "key%u", i);
      check_string(eph->weak_pair.key, buf);
    }
    {
      char val[10];
      char key[10];
      sprintf(val, "val%u", i);
      sprintf(key, "key%u", (i + 1) % n);
      asserts(eph->weak_pair.value->header.type == TYPE_PAIR,
              "TYPE_PAIR");
      check_string(eph->weak_pair.value->pair.car, val);
      check_string(eph->weak_pair.value->pair.cdr, key);
    }
  }
}

static void test_two_pairs(mmp mm, oop s[ROOT_COUNT])
{
  comment("test_two_pairs");
  s[0] = string_from_cstr(mm->amc_ap, "a");
  s[1] = string_from_cstr(mm->amc_ap, "b");
  s[2] = string_from_cstr(mm->amc_ap, "c");
  s[3] = make_weak_pair(mm->eph_ap, s[0], s[1]);
  s[4] = make_weak_pair(mm->eph_ap, s[2], s[0]);
  s[0] = NULL;
  s[1] = NULL;
  die(mps_arena_collect(mm->arena), "mps_arena_collect");
  asserts(s[3]->weak_pair.key != NULL, "key1 reachable");
  asserts(s[3]->weak_pair.value != NULL, "value1 reachable");
  asserts(s[4]->weak_pair.key != NULL, "key2 reachable");
  asserts(s[4]->weak_pair.value != NULL, "value2 reachable");
  check_string(s[3]->weak_pair.key, "a");
  check_string(s[3]->weak_pair.value, "b");
  check_string(s[4]->weak_pair.key, "c");
  check_string(s[4]->weak_pair.value, "a");
}

static void test(mmp mm, void* closure)
{
  typedef void (*fun)(mmp, oop s[ROOT_COUNT]);
  fun funs[] = { test_reachable_key, test_unreachable_key,
                 test_key_eq_value,  test_value_unreachable,
                 test_chain,         test_two_pairs };
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

/* libcbt: MPS LIBRARY CALLBACK TEST
 *
 * $Header$
 * Copyright (C) 2005 Ravenbrook Limited.  See end of file for license.
 *
 * This is a simple test of the MPS Library Callback interface
 * (mpslibcb.h). */

#include "mps.h"
#include "mpsavm.h"
#include "mpslib.h"
#include "mpslibcb.h"

#include "testlib.h"

#include <stdio.h>
#include <stdlib.h>

void libcbt_assert_fail(const char *);
mps_clock_t libcbt_clock(void);

int main(void)
{
  int res;
  int defects = 0;
  mps_arena_t arena;

  res = mps_lib_callback_register("not a callback", (void(*)(void))0);
  if(MPS_RES_OK == res) {
    printf("mps_lib_callback_register claims to successfully register\n"
      "an interface that does not exist.\n");
    ++ defects;
  }
  die(mps_lib_callback_register("mps_lib_assert_fail",
    (void(*)(void))libcbt_assert_fail),
    "register assert_fail");
  /* The following functions are registered in the order that you get by
   * providing no functions and then providing functions as they are
   * required by assertionn failures.
   * Interestingly, for this very simple test, only mps_clock is
   * required. */
  die(mps_lib_callback_register("mps_clock",
    (mps_lib_function_t)libcbt_clock),
    "register clock");
  die(mps_arena_create(&arena, mps_arena_class_vm(), 1000*1000),
    "mps_arena_create");
  if(defects) {
    printf("Conclusion: Defects detected.\n");
  } else {
    printf("Conclusion: Failed to find any defects.\n");
  }
  return 0;
}

void libcbt_assert_fail(const char *message)
{
  fflush(stdout);
  fprintf(stderr, "\nMPS ASSERTION FAILURE (TEST): %s\n", message);
  fflush(stderr);
  abort();
}

mps_clock_t libcbt_clock(void)
{
  static mps_clock_t c = 0;

  ++ c;
  return c;
}

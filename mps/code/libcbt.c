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
#include "mpslib.h"

#include <stdio.h>
#include <stdlib.h>

void libcbt_assert_fail(const char *);
mps_clock_t libcbt_clock(void);

int main(int argc, char *argv[])
{
  int res;
  mps_arena_t arena;

  res = mps_lib_callback_register("not a callback", (void(*)(void))0);
  if(MPS_RES_OK == res) {
    error("mps_lib_callback_register claims to successfully register\n"
          "an interface that does not exist.\n");
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
  die(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1000*1000),
    "mps_arena_create");

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2005-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

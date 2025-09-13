/* poolncv.c: NULL POOL COVERAGE TEST
 *
 *  $Id$
 *  Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "mpsavm.h"
#include "mpslib.h"
#include "pooln.h"
#include "testlib.h"

#include <stdio.h> /* printf */


static void testit(ArenaClass klass, ArgList args)
{
  Arena arena;
  Pool pool;
  Res res;
  Addr p;

  die(ArenaCreate(&arena, klass, args), "ArenaCreate");

  die(PoolCreate(&pool, arena, PoolClassN(), argsNone), "PoolNCreate");
  res = PoolAlloc(&p, pool, 1);
  if (res == ResOK) {
    error("Error: Unexpectedly succeeded in"
          "allocating block from PoolN\n");
  }
  PoolDescribe(pool, mps_lib_get_stdout(), 0);
  PoolDestroy(pool);
  ArenaDestroy(arena);
}


int main(int argc, char *argv[])
{
  testlib_init(argc, argv);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 600000);
    testit((ArenaClass)mps_arena_class_vm(), args);
  } MPS_ARGS_END(args);
  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

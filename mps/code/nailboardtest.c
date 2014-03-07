/* nailboardtest.c: NAILBOARD TEST
 *
 * $Id: //info.ravenbrook.com/project/mps/branch/2014-01-15/nailboard/code/fotest.c#1 $
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 */

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"
#include "bt.h"
#include "nailboard.h"


static void test(mps_arena_t arena)
{
  BT bt;
  Nailboard board;
  Align align;
  Count nails;
  Addr base, limit;
  Index i, j, k;

  align = 1 << (rnd() % 10);
  nails = 1 << (rnd() % 16);
  base = AddrAlignUp(0, align);
  limit = AddrAdd(base, nails * align);

  die(BTCreate(&bt, arena, nails), "BTCreate");
  die(NailboardCreate(&board, arena, align, base, limit), "NailboardCreate");

  for (i = 0; i < nails / 8; ++i) {
    j = rnd() % nails;
    BTSet(bt, j);
    NailboardSet(board, AddrAdd(base, j * align));
    for (k = 0; k < nails / 8; ++k) {
      Index b, l;
      b = rnd() % nails;
      l = b + rnd() % (nails - b) + 1;
      if (BTIsResRange(bt, b, l)
          != NailboardIsResRange(board, AddrAdd(base, b * align),
                                 AddrAdd(base, l * align)))
      {
        NailboardIsResRange(board, AddrAdd(base, b * align),
                            AddrAdd(base, l * align));
      }
    }
  }
}

int main(int argc, char **argv)
{
  mps_arena_t arena;

  randomize(argc, argv);
  mps_lib_assert_fail_install(assert_die);
  die(mps_arena_create(&arena, mps_arena_class_vm(), 1024 * 1024),
      "mps_arena_create");

  test(arena);

  mps_arena_destroy(arena);
  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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


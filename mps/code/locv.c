/* impl.c.locv: LEAF OBJECT POOL CLASS COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This is (not much of) a coverage test for the Leaf Object
 * pool (PoolClassLO).
 */

#include "testlib.h"
#include "mps.h"
#include "mpsclo.h"
#include "mpsavm.h"


#define testArenaSIZE   ((size_t)16<<20)

static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t skip(mps_addr_t object);
static void move(mps_addr_t object, mps_addr_t to);
static mps_addr_t isMoved(mps_addr_t object);
static void copy(mps_addr_t old, mps_addr_t new);
static void pad(mps_addr_t base, size_t size);

static mps_fmt_A_s locv_fmt =
  {
    (mps_align_t)4,
    scan,
    skip,
    copy,
    move,
    isMoved,
    pad
  };

static mps_addr_t roots[4];


int main(void)
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_fmt_t format;
  mps_ap_t ap;
  mps_addr_t p;
  mps_root_t root;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  die(mps_root_create_table(&root, arena, MPS_RANK_EXACT,
                            (mps_rm_t)0,
                            roots, (sizeof(roots)/sizeof(*roots))),
      "RootCreate");

  die(mps_fmt_create_A(&format, arena, &locv_fmt), "FormatCreate");

  die(mps_pool_create(&pool, arena, mps_class_lo(), format), "LOCreate");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "APCreate");

  die(mps_reserve(&p, ap, (size_t)4), "mps_reserve 4");
  *(mps_word_t *)p = 4;
  cdie(mps_commit(ap, p, (size_t)4), "commit 4");
  die(mps_reserve(&roots[1], ap, (size_t)8), "mps_reserve 8");
  p = roots[1];
  *(mps_word_t *)p = 8;
  cdie(mps_commit(ap, p, (size_t)8), "commit 8");
  die(mps_reserve(&p, ap, (size_t)4096), "mps_reserve 4096");
  *(mps_word_t *)p = 4096;
  cdie(mps_commit(ap, p, (size_t)4096), "commit 4096");
  die(mps_reserve(&p, ap, (size_t)4), "mps_reserve last");
  *(mps_word_t *)p = 4;
  cdie(mps_commit(ap, p, (size_t)4), "commit last");

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  mps_root_destroy(root);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}


static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  testlib_unused(ss);
  testlib_unused(base);
  testlib_unused(limit);
  die(MPS_RES_FAIL, "Error in Test, scan called unexpectedly");
  return MPS_RES_FAIL;
}


static mps_addr_t skip(mps_addr_t object)
{
  size_t bytes;

  bytes = (size_t)(*(mps_word_t *)object);

  return (mps_addr_t)((char *)object + bytes);
}


static void move(mps_addr_t object, mps_addr_t to)
{
  testlib_unused(object);
  testlib_unused(to);
  cdie(0, "move");
}


static mps_addr_t isMoved(mps_addr_t object)
{
  testlib_unused(object);
  cdie(0, "isMoved");
  return (mps_addr_t)NULL;
}


static void copy(mps_addr_t old, mps_addr_t new)
{
  testlib_unused(old);
  testlib_unused(new);
  cdie(0, "copy");
}


static void pad(mps_addr_t base, size_t size)
{
  testlib_unused(base);
  testlib_unused(size);
  cdie(0, "pad");
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

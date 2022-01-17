/* locv.c: LEAF OBJECT POOL CLASS COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * This is (not much of) a coverage test for the Leaf Object
 * pool (PoolClassLO).
 */

#include "testlib.h"
#include "mpslib.h"
#include "mps.h"
#include "mpsclo.h"
#include "mpsavm.h"

#include <stdio.h> /* printf */


#define testArenaSIZE   ((size_t)16<<20)

static mps_res_t scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit);
static mps_addr_t skip(mps_addr_t object);
static void move(mps_addr_t object, mps_addr_t to);
static mps_addr_t isMoved(mps_addr_t object);
static void copy(mps_addr_t old, mps_addr_t new);
static void pad(mps_addr_t base, size_t size);

static void stepper(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool,
                    void *p, size_t s);

static mps_fmt_A_s locv_fmt =
  {
    (mps_align_t)0,  /* .fmt.align.delayed: to be filled in */
    scan,
    skip,
    copy,
    move,
    isMoved,
    pad
  };

static mps_addr_t roots[4];


/* area_scan -- area scanning function for mps_pool_walk */

static mps_res_t area_scan(mps_ss_t ss, void *base, void *limit, void *closure)
{
  unsigned long *count = closure;
  testlib_unused(ss);
  while (base < limit) {
    mps_addr_t prev = base;
    ++ *count;
    base = skip(base);
    Insist(prev < base);
  }
  Insist(base == limit);
  return MPS_RES_OK;
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_fmt_t format;
  mps_ap_t ap;
  mps_addr_t p;
  mps_root_t root;

  testlib_init(argc, argv);

  locv_fmt.align = sizeof(void *);  /* .fmt.align.delayed */

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  die(mps_root_create_table(&root, arena, mps_rank_exact(),
                            (mps_rm_t)0,
                            roots, (sizeof(roots)/sizeof(*roots))),
      "RootCreate");

  die(mps_fmt_create_A(&format, arena, &locv_fmt), "FormatCreate");

  die(mps_pool_create(&pool, arena, mps_class_lo(), format), "LOCreate");

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "APCreate");

  die(mps_reserve(&p, ap, sizeof(void *)), "mps_reserve min");
  *(mps_word_t *)p = sizeof(void *);
  cdie(mps_commit(ap, p, sizeof(void *)), "commit min");

  die(mps_reserve(&roots[1], ap, 2*sizeof(void *)), "mps_reserve 2*min");
  p = roots[1];
  *(mps_word_t *)p = 2*sizeof(void *);
  cdie(mps_commit(ap, p, 2*sizeof(void *)), "commit 2*min");

  die(mps_reserve(&p, ap, (size_t)4096), "mps_reserve 4096");
  *(mps_word_t *)p = 4096;
  cdie(mps_commit(ap, p, (size_t)4096), "commit 4096");

  die(mps_reserve(&p, ap, sizeof(void *)), "mps_reserve last");
  *(mps_word_t *)p = sizeof(void *);
  cdie(mps_commit(ap, p, sizeof(void *)), "commit last");

  mps_arena_park(arena);
  {
    size_t count = 0;
    mps_arena_formatted_objects_walk(arena, stepper, &count, 0);
    cdie(count == 4, "stepped 4 objects");
  }
  {
    size_t count = 0;
    die(mps_pool_walk(pool, area_scan, &count), "mps_pool_walk");
    cdie(count == 4, "walk 4 objects");
  }

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  mps_root_destroy(root);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
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

static void stepper(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool,
                    void *p, size_t s)
{
  size_t *pcount;

  testlib_unused(addr);
  testlib_unused(fmt);
  testlib_unused(pool);
  testlib_unused(s);

  pcount = p;
  *pcount += 1;
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

/* impl.c.locv: LEAF OBJECT POOL CLASS COVERAGE TEST
 *
 * $HopeName: MMsrc!locv.c(trunk.15) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
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

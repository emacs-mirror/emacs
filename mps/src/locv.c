/* impl.c.locv: Leaf Object Pool Class Coverage Test
 *
 * $HopeName: !locv.c(trunk.15) $
 *
 * Copyright (C) 1996,1997, 1998 Harlequin Group, all rights reserved
 *
 * This is (not much of) a coverage test for the Leaf Object
 * pool (PoolClassLO).
 *
 * .hack.stdio: On platform.sus8lc the assert macro in assert.h uses
 * stderr without declaring it.  Therefore using the assert macro will
 * not work unless stderr has been declared by #including <stdio.h>.
 */

#include "testlib.h"
#include "mps.h"
#include "mpsclo.h"
#include "mpsavm.h"

#include <assert.h>
#ifdef MPS_PF_SUS8LC
#include <stdio.h>	/* .hack.stdio */
#endif


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
  mps_bool_t b;
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

  die(mps_reserve(&p, ap, (size_t)4), "mps_reserve");
  *(mps_word_t *)p = 4;
  b = mps_commit(ap, p, (size_t)4);
  assert(b);
  die(mps_reserve(&roots[1], ap, (size_t)8), "mps_reserve");
  p = roots[1];
  *(mps_word_t *)p = 8;
  b = mps_commit(ap, p, (size_t)8);
  assert(b);
  die(mps_reserve(&p, ap, (size_t)4096), "mps_reserve");
  *(mps_word_t *)p = 4096;
  b = mps_commit(ap, p, (size_t)4096);
  assert(b);
  die(mps_reserve(&p, ap, (size_t)4), "mps_reserve");
  *(mps_word_t *)p = 4;
  b = mps_commit(ap, p, (size_t)4);
  assert(b);

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  mps_root_destroy(root);
  mps_arena_destroy(arena);

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
  assert(0);
}


static mps_addr_t isMoved(mps_addr_t object)
{
  testlib_unused(object);
  assert(0);
  return (mps_addr_t)NULL;
}


static void copy(mps_addr_t old, mps_addr_t new)
{
  testlib_unused(old);
  testlib_unused(new);
  assert(0);
}


static void pad(mps_addr_t base, size_t size)
{
  testlib_unused(base);
  testlib_unused(size);
  assert(0);
}

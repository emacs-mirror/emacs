/* impl.c.awlut: POOL CLASS AWL UNIT TEST
 *
 * $HopeName: MMsrc!awlut.c(trunk.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * Any MPS developer, any interested QA.
 *
 * DESIGN
 *
 * .design: see design.mps.poolawl.test.*
 */


#include "mps.h"
#include "mpscawl.h"
#include "mpsclo.h"
#include "fmtdy.h"
#include "testlib.h"

static void *test(void *v, size_t s)
{
  mps_space_t space;
  mps_pool_t leafpool;
  mps_pool_t tablepool;
  mps_fmt_t dylanfmt;
  mps_ap_t leafap, exactap, weakap;

  space = (mps_space_t)v;

  die(mps_fmt_create_A(&dylanfmt, space, dylan_fmt_A()),
      "Format Create\n");
  die(mps_pool_create(&leafpool, space, mps_class_lo(), dylanfmt),
      "Leaf Pool Create\n");
  die(mps_pool_create(&tablepool, space, mps_class_awl(), dylanfmt),
      "Table Pool Create\n");
  die(mps_ap_create(&leafap, leafpool, MPS_RANK_EXACT),
      "Leaf AP Create\n");
  die(mps_ap_create(&exactap, tablepool, MPS_RANK_EXACT),
      "Exact AP Create\n");
  die(mps_ap_create(&weakap, tablepool, MPS_RANK_WEAK),
      "Weak AP Create\n");

  mps_ap_destroy(weakap);
  mps_ap_destroy(exactap);
  mps_ap_destroy(leafap);
  mps_pool_destroy(tablepool);
  mps_pool_destroy(leafpool);
  mps_fmt_destroy(dylanfmt);

  return NULL;
}


int main(void)
{
  mps_space_t space;
  mps_thr_t thread;
  void *r;

  die(mps_space_create(&space), "space_create");
  die(mps_thread_reg(&thread, space), "thread_reg");
  mps_tramp(&r, test, space, 0);
  mps_thread_dereg(thread);
  mps_space_destroy(space);

  return 0;
}

/* impl.c.awlut: POOL CLASS AWL UNIT TEST
 *
 * $HopeName$
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
#include "testlib.h"

static void *test(void *v, size_t s)
{
  mps_space_t space;
  mps_pool_t leafpool;
  mps_pool_t tablepool;
  mps_ap_t leafap, exactap, weakap;

  space = (mps_space_t)v;

  leafpool = tablepool = NULL;
  leafap = exactap = weakap = NULL;
  (void)(leafpool, tablepool, leafap, exactap, weakap, 0);

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

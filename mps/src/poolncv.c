/*  impl.c.poolncv
 *
 *                   NULL POOL COVERAGE TEST
 *
 *  $HopeName: MMsrc!poolncv.c(trunk.1) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 */


#include "std.h"
#include "pool.h"
#include "pooln.h"
#include "space.h"

#include "testlib.h"

#include <stdio.h>

int
main(void)
{
  Bool eflag = FALSE;
  Space space;
  Pool pool;
  Error e;
  Addr p;

  die(SpaceCreate(&space), "SpaceCreate");

  die(PoolCreate(&pool, PoolClassN(), space), "PoolNCreate");
  e = PoolAlloc(&p, pool, 1);
  if(e == ErrSUCCESS) {
    fprintf(stderr,
	    "Error:  Unexpectedly succeeded in"
	    "allocating block from PoolN\n");
    eflag = TRUE;
  }
  PoolDestroy(pool);
  SpaceDestroy(space);
  if(eflag) {
    fprintf(stderr, "Conclusion:  Defects found.\n");
  } else {
    fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  }
  return 0;
}




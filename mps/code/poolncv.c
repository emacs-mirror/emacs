/*  impl.c.poolncv: NULL POOL COVERAGE TEST
 *
 *  $HopeName: MMsrc!poolncv.c(trunk.8) $
 *  Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 */

#include "mpm.h"
#include "pooln.h"
#include "mpsavm.h"
#include "testlib.h"


static Bool testit(ArenaClass class, ...)
{
  Bool eflag = FALSE;
  Arena arena;
  Pool pool;
  Res res;
  Addr p;
  va_list args;

  va_start(args, class);
  die(ArenaCreateV(&arena, class, args), "ArenaCreate");
  va_end(args);

  die(PoolCreate(&pool, arena, PoolClassN()), "PoolNCreate");
  res = PoolAlloc(&p, pool, 1, /* withReservoirPermit */ FALSE);
  if(res == ResOK) {
    fprintf(stderr,
            "Error:  Unexpectedly succeeded in"
            "allocating block from PoolN\n");
    eflag = TRUE;
  }
  PoolDestroy(pool);
  ArenaDestroy(arena);

  return eflag;
}


int main(void)
{
  if(testit((ArenaClass)mps_arena_class_vm(), (Size)200000)) {
    fprintf(stderr, "Conclusion:  Defects found.\n");
  } else {
    fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  }
  return 0;
}

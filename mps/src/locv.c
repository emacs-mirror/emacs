/*  impl.c.locv
 *
 *            Leaf Object Pool Class Coverage Test
 *
 *  $HopeName: MMsrc!locv.c(trunk.1) $
 *
 *  Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 *  This is (not much of) a coverage test for the Leaf Object
 *  pool (PoolClassLO).
 */


#include "std.h"
#include "lo.h"
#include "testlib.h"
#include "coll.h"
#include "format.h"
#include "pool.h"
#include "poolclas.h"
#include "sched.h"
#include "space.h"



static Error scan(ScanState ss, Addr base, Addr limit);
static Addr skip(Addr object);
static void move(Addr object, Addr to);
static Addr isMoved(Addr object);
static void copy(Addr object, Addr to);

static Addr roots[4];

int
main(void)
{
  Space space;
  Pool pool;
  Format format;
  Buffer buffer;
  Addr p;
  Bool b;
  Coll coll;
  int i;
  Root root;

  die(SpaceCreate(&space), "SpaceCreate");
  die(RootCreateTable(&root, space, RefRankEXACT,
                      RootFIXABLE || RootMUTABLE || RootATOMIC,
                      roots, roots + (sizeof(roots)/sizeof(*roots))),
      "RootCreate");

  die(FormatCreate(&format, space, (Addr)4, scan, skip,
		   move, isMoved, copy), "FormatCreate");
  die(PoolCreate(&pool, PoolClassLO(), space, format), "LOCreate");

  die(BufferCreate(&buffer, pool), "BufferCreate");

  die(BufferReserve(&p, buffer, (Size)4), "BufferReserve");
  *(Addr *)p = 4;
  b = BufferCommit(buffer, p, (Size)4);
  AVER(b);
  die(BufferReserve(&roots[1], buffer, (Size)8), "BufferReserve");
  p = roots[1];
  *(Addr *)p = 8;
  b = BufferCommit(buffer, p, (Size)8);
  AVER(b);
  die(BufferReserve(&p, buffer, (Size)4096), "BufferReserve");
  *(Addr *)p = 4096;
  b = BufferCommit(buffer, p, (Size)4096);
  AVER(b);
  die(BufferReserve(&p, buffer, (Size)4), "BufferReserve");
  *(Addr *)p = 4;
  b = BufferCommit(buffer, p, (Size)4);
  AVER(b);

  die(CollCreate(&coll, pool), "CollCreate");
  die(SchedProcAdd(SpaceSched(space), CollProc, coll, 0),
      "SchedProcAdd");

  /*  .collect.finish: impossible to tell when a collection
   *  is finished */
  for(i = 0; i < 10; ++i) {
    SchedRun(SpaceSched(space));
  }

  BufferDestroy(buffer);
  PoolDestroy(pool);
  RootDestroy(root);
  SpaceDestroy(space);

  return 0;
}

static
Error
scan(ScanState ss, Addr base, Addr limit)
{
  NOTREACHED;
  return ErrFAILURE;
}

static
Addr
skip(Addr object)
{
  Addr bytes;

  bytes = *(Addr *)object;

  return object + bytes;
}

static
void
move(Addr object, Addr to)
{
  NOTREACHED;
}

static
Addr
isMoved(Addr object)
{
  NOTREACHED;
  return ErrFAILURE;
}

static
void
copy(Addr object, Addr to)
{
  NOTREACHED;
}

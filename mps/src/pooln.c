/*  impl.c.pooln
 *
 *                         NULL POOL
 *
 *  $HopeName: MMsrc!pooln.c(trunk.6) $
 *
 *  Copyright(C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the null pool class.  Begin null it
 *  all functions are implemented in a trivial manner.
 */


#include "std.h"
#include "pooln.h"
#include "poolnst.h"
#include "buffer.h"
#include "lib.h"
#include "pool.h"
#include "poolar.h"
#include "poolclas.h"
#include "ref.h"
#include "space.h"
#include "trace.h"
#include "prot.h"
#include <stdarg.h>
#include <stddef.h>

SRCID("$HopeName: MMsrc!pooln.c(trunk.6) $");


/*  Class's methods  */

static Error create(Pool *poolReturn, Space space, va_list arg);
static void destroy(Pool pool);
static Error alloc(Addr *pReturn, Pool pool, Size size);
static void free_(Pool pool, Addr old, Size size);
static Error bufferCreate(Buffer *bufReturn, Pool pool);
static void bufferDestroy(Pool pool, Buffer buf);
static Error bufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size);
static Bool bufferTrip(Pool pool, Buffer buffer, Addr p, Size size);
static void bufferExpose(Pool pool, Buffer buffer);
static void bufferCover(Pool pool, Buffer buffer);
static Error describe(Pool pool, LibStream stream);
static Error condemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti);
static void mark(Pool pool, Space space, TraceId ti);
static Error scan(ScanState ss, Pool pool, Bool *finishedReturn);
static Error fix(Pool pool, ScanState ss, Arena arena, Ref *refIO);
static void reclaim(Pool pool, Space space, TraceId ti);
static void access(Pool pool, Addr seg, ProtMode mode);


/*  Class Structure  */
static PoolClassStruct PoolClassNStruct;


PoolClass PoolClassN(void)
{
  PoolClassInit(&PoolClassNStruct,
                "N",
                sizeof(PoolNStruct), offsetof(PoolNStruct, poolStruct),
                create, destroy,
                alloc, free_,
                bufferCreate, bufferDestroy,
                bufferFill, bufferTrip,
                bufferExpose, bufferCover,
                condemn, mark, scan,
                fix, reclaim,
                access,
                describe);
  return &PoolClassNStruct;
}


Bool PoolNIsValid(PoolN poolN, ValidationType validParam)
{
  AVER(poolN != NULL);
  AVER(ISVALIDNESTED(Pool, &poolN->poolStruct));
  AVER(poolN->poolStruct.class == &PoolClassNStruct);

  return TRUE;
}


static Error create(Pool *poolReturn, Space space, va_list arg)
{
  PoolN poolN;
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  UNUSED(arg);

  e = PoolNCreate(&poolN, space);
  if(e != ErrSUCCESS) {
    return e;
  }
  *poolReturn = PoolNPool(poolN);
  return ErrSUCCESS;
}

Error PoolNCreate(PoolN *poolNReturn, Space space)
{
  Error e;
  Pool control;
  PoolN poolN;

  AVER(poolNReturn != NULL);
  AVER(ISVALID(Space, space));

  control = SpaceControlPool(space);
  e = PoolAlloc((Addr *)&poolN, control, (Size)sizeof(PoolNStruct));
  if(e != ErrSUCCESS)
    goto failAlloc;
  e = PoolNInit(poolN, space);
  if(e != ErrSUCCESS)
    goto failInit;

  *poolNReturn = poolN;
  return ErrSUCCESS;

  failInit:
    PoolFree(control, (Addr)poolN, (Size)sizeof(PoolNStruct));
  failAlloc:
    return e;
}

Error PoolNInit(PoolN poolN, Space space)
{
  AVER(poolN != NULL);
  AVER(ISVALID(Space, space));

  PoolInit(&poolN->poolStruct, space, PoolClassN());
  AVER(ISVALID(PoolN, poolN));

  return ErrSUCCESS;
}

static void destroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  PoolNDestroy(PARENT(PoolNStruct, poolStruct, pool));
}

void PoolNDestroy(PoolN poolN)
{
  Pool control;

  AVER(ISVALID(PoolN, poolN));
  control = SpaceControlPool(PoolSpace(PoolNPool(poolN)));
  PoolNFinish(poolN);
  PoolFree(control, (Addr)poolN, (Size)sizeof(PoolNStruct));
}

void PoolNFinish(PoolN poolN)
{
  AVER(ISVALID(PoolN, poolN));
  PoolFinish(PoolNPool(poolN));
}

Pool (PoolNPool)(PoolN poolN)
{
  AVER(ISVALID(PoolN, poolN));
  return &poolN->poolStruct;
}

static Error alloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(size > 0);

  return ErrLIMIT;  /* limit of nil blocks exceeded */
}

static void free_(Pool pool, Addr old, Size size)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(old != (Addr)0);
  AVER(size > 0);

  NOTREACHED;  /* can't allocate, should never free */
}

static Error bufferCreate(Buffer *bufReturn, Pool pool)
{
  AVER(bufReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
#ifndef DEBUG
  UNUSED(bufReturn);
  UNUSED(pool);
#endif /* DEBUG */

  return ErrLIMIT;  /* limit of nil buffers exceeded */
}

static void bufferDestroy(Pool pool, Buffer buf)
{
  AVER(ISVALID(Pool, pool));
  AVER(ISVALID(Buffer, buf));

  NOTREACHED;  /* can't create, so shouldn't destroy */
}

static Error bufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  AVER(ISVALID(Pool, pool));
  AVER(ISVALID(Buffer, buffer));
  AVER(size > 0);
  AVER(pReturn != NULL);

  NOTREACHED;	/* can't create buffers, so shouldn't fill them */
  return ErrUNIMPL;
}

static Bool bufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  AVER(ISVALID(Pool, pool));
  AVER(ISVALID(Buffer, buffer));
  AVER(p != 0);
  AVER(size > 0);

  NOTREACHED;	/* can't create buffers, so they shouldn't trip */
  return FALSE;
}


static void bufferExpose(Pool pool, Buffer buffer)
{
  AVER(ISVALID(Pool, pool));
  AVER(ISVALID(Buffer, buffer));

  NOTREACHED;	/* can't create buffers, so shouldn't expose them */
}


static void bufferCover(Pool pool, Buffer buffer)
{
  AVER(ISVALID(Pool, pool));
  AVER(ISVALID(Buffer, buffer));

  NOTREACHED;	/* can't create buffers, so shouldn't cover them */
}


static Error describe(Pool pool, LibStream stream)
{
  PoolN poolN;

  UNUSED(stream);

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);

  poolN = PARENT(PoolNStruct, poolStruct, pool);
  AVER(ISVALID(PoolN, poolN));

  return ErrSUCCESS;
}


static Error condemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti)
{
  AVER(condemnedReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Space, space));
#ifndef DEBUG
  UNUSED(pool);
  UNUSED(space);
#endif /* DEBUG */

  return ErrSUCCESS;
}

static void mark(Pool pool, Space space, TraceId ti)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Space, space));
}

static Error scan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVER(finishedReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(ScanState, ss));

  return ErrSUCCESS;
}

static Error fix(Pool pool, ScanState ss, Arena arena, Ref *refIO)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(ScanState, ss));
  AVER(ISVALID(Arena, arena));
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ErrFAILURE;
}

static void reclaim(Pool pool, Space space, TraceId ti)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Space, space));
#ifndef DEBUG
  UNUSED(pool);
  UNUSED(space);
#endif
  /* all unmarked and condemned objects reclaimed */
}

static void access(Pool pool, Addr seg, ProtMode mode)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  UNUSED(seg);
  UNUSED(mode);
  /* deal with access to segment */
}

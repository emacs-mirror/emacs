/*  impl.c.pooln
 *
 *                         NULL POOL
 *
 *  $HopeName: MMsrc!pooln.c(MMdevel_restr.4) $
 *
 *  Copyright(C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the null pool class.  Begin null it
 *  all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(MMdevel_restr.4) $");


typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* and that's it */
} PoolNStruct;


/*  Class's methods  */

static Res create(Pool *poolReturn, Space space, va_list arg);
static void destroy(Pool pool);
static Res alloc(Addr *pReturn, Pool pool, Size size);
static void free_(Pool pool, Addr old, Size size);
static Res bufferCreate(Buffer *bufReturn, Pool pool);
static void bufferDestroy(Pool pool, Buffer buf);
static Res bufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size);
static Bool bufferTrip(Pool pool, Buffer buffer, Addr p, Size size);
static void bufferExpose(Pool pool, Buffer buffer);
static void bufferCover(Pool pool, Buffer buffer);
static Res describe(Pool pool, Lib_FILE *stream);
static Res condemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti);
static void mark(Pool pool, Space space, TraceId ti);
static Res scan(ScanState ss, Pool pool, Bool *finishedReturn);
static Res fix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
static void reclaim(Pool pool, Space space, TraceId ti);
static void access(Pool pool, Seg seg, AccessSet mode);


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


Bool PoolNCheck(PoolN poolN)
{
  CHECKL(poolN != NULL);
  CHECKD(Pool, &poolN->poolStruct);
  CHECKL(poolN->poolStruct.class == &PoolClassNStruct);

  return TRUE;
}


static Res create(Pool *poolReturn, Space space, va_list arg)
{
  PoolN poolN;
  Res res;

  AVER(poolReturn != NULL);
  AVERT(Space, space);
  UNUSED(arg);

  res = PoolNCreate(&poolN, space);
  if(res != ResOK) {
    return res;
  }
  *poolReturn = PoolNPool(poolN);
  return ResOK;
}

Res PoolNCreate(PoolN *poolNReturn, Space space)
{
  Res res;
  PoolN poolN;

  AVER(poolNReturn != NULL);
  AVERT(Space, space);

  res = SpaceAlloc((Addr *)&poolN, space, (Size)sizeof(PoolNStruct));
  if(res != ResOK)
    goto failAlloc;
  res = PoolNInit(poolN, space);
  if(res != ResOK)
    goto failInit;

  *poolNReturn = poolN;
  return ResOK;

failInit:
  SpaceFree(space, (Addr)poolN, (Size)sizeof(PoolNStruct));
failAlloc:
  return res;
}

Res PoolNInit(PoolN poolN, Space space)
{
  AVER(poolN != NULL);
  AVERT(Space, space);

  PoolInit(&poolN->poolStruct, space, PoolClassN());
  AVERT(PoolN, poolN);

  return ResOK;
}

static void destroy(Pool pool)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  PoolNDestroy(PARENT(PoolNStruct, poolStruct, pool));
}

void PoolNDestroy(PoolN poolN)
{
  Space space;
  AVERT(PoolN, poolN);
  space = PoolSpace(PoolNPool(poolN));
  PoolNFinish(poolN);
  SpaceFree(space, (Addr)poolN, (Size)sizeof(PoolNStruct));
}

void PoolNFinish(PoolN poolN)
{
  AVERT(PoolN, poolN);
  PoolFinish(PoolNPool(poolN));
}

Pool (PoolNPool)(PoolN poolN)
{
  AVERT(PoolN, poolN);
  return &poolN->poolStruct;
}

static Res alloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVER(size > 0);

  return ResLIMIT;  /* limit of nil blocks exceeded */
}

static void free_(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVER(old != (Addr)0);
  AVER(size > 0);

  NOTREACHED;  /* can't allocate, should never free */
}

static Res bufferCreate(Buffer *bufReturn, Pool pool)
{
  AVER(bufReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);

  return ResLIMIT;  /* limit of nil buffers exceeded */
}

static void bufferDestroy(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);

  NOTREACHED;  /* can't create, so shouldn't destroy */
}

static Res bufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(pReturn != NULL);

  NOTREACHED;   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static Bool bufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(p != 0);
  AVER(size > 0);

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
  return FALSE;
}


static void bufferExpose(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't expose them */
}


static void bufferCover(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't cover them */
}


static Res describe(Pool pool, Lib_FILE *stream)
{
  PoolN poolN;

  UNUSED(stream);

  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);

  poolN = PARENT(PoolNStruct, poolStruct, pool);
  AVERT(PoolN, poolN);

  return ResOK;
}


static Res condemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti)
{
  AVER(condemnedReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);

  return ResOK;
}

static void mark(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);
}

static Res scan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVER(finishedReturn != NULL);
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(ScanState, ss);

  return ResOK;
}

static Res fix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ResFAIL;
}

static void reclaim(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Space, space);
  /* all unmarked and condemned objects reclaimed */
}

static void access(Pool pool, Seg seg, AccessSet mode)
{
  AVERT(Pool, pool);
  AVER(pool->class == &PoolClassNStruct);
  AVERT(Seg, seg);
  UNUSED(mode);
  /* deal with access to segment */
}

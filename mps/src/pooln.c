/* impl.c.pooln: NULL POOL CLASS
 *
 * $HopeName: MMsrc!pooln.c(trunk.24) $
 * Copyright (C) 1997 Harlequin Group plc.  All rights reserved.
 *
 * .readership: MPS developers
 */

#include "pooln.h"
#include "mpm.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(trunk.24) $");


typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* and that's it */
} PoolNStruct;

#define PoolPoolN(pool) PARENT(PoolNStruct, poolStruct, pool)


static Res NInit(Pool pool, va_list args)
{
  PoolN poolN = PoolPoolN(pool);

  UNUSED(args);
  
  /* Initialize pool-specific structures. */

  AVERT(PoolN, poolN);

  return ResOK;
}


static void NFinish(Pool pool)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  /* Finish pool-specific structures. */
}


Pool (PoolNPool)(PoolN poolN)
{
  AVERT(PoolN, poolN);
  return &poolN->poolStruct;
}


static Res NAlloc(Addr *pReturn, Pool pool, Size size,
                  Bool withReservoirPermit)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVER(pReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  return ResLIMIT;  /* limit of nil blocks exceeded */
}


static void NFree(Pool pool, Addr old, Size size)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVER(old != (Addr)0);
  AVER(size > 0);

  NOTREACHED;  /* can't allocate, should never free */
}


static Res NBufferInit(Pool pool, Buffer buffer, va_list args)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(buffer); UNUSED(args);

  return ResLIMIT;  /* limit of nil buffers exceeded */
}


static void NBufferFinish(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));

  NOTREACHED;  /* can't create, so shouldn't destroy */
}


static Res NBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size,
                       Bool withReservoirPermit)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);
  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  NOTREACHED;   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}


static void NBufferEmpty(Pool pool, Buffer buffer, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(SegCheck(seg));

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
}

static Res NDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(stream); /* @@@@ should output something here */

  return ResOK;
}


static Res NWhiten(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  
  NOTREACHED; /* pool doesn't have any actions */

  return ResUNIMPL;
}


static void NGrey(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
}


static void NBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
}


static Res NScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  PoolN poolN;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);
  AVERT(Seg, seg);

  return ResOK;
}


static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(ScanState, ss);
  UNUSED(refIO);
  AVERT(Seg, seg);
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ResFAIL;
}


static void NReclaim(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  /* all unmarked and white objects reclaimed */
}


DEFINE_POOL_CLASS(NPoolClass, this)
{
  INHERIT_CLASS(this, AbstractPoolClass);
  this->name = "N";
  this->size = sizeof(PoolNStruct);
  this->offset = offsetof(PoolNStruct, poolStruct);
  this->attr = AttrSCAN | AttrALLOC | AttrFREE | AttrBUF |
                 AttrBUF_RESERVE | AttrGC;
  this->init = NInit;
  this->finish = NFinish;
  this->alloc = NAlloc;
  this->free = NFree;
  this->bufferInit = NBufferInit;
  this->bufferFill = NBufferFill;
  this->bufferEmpty = NBufferEmpty;
  this->bufferFinish = NBufferFinish;
  this->whiten = NWhiten;
  this->grey = NGrey;
  this->blacken = NBlacken;
  this->scan = NScan;
  this->fix = NFix;
  this->fixEmergency = NFix;
  this->reclaim = NReclaim;
  this->describe = NDescribe;
}


PoolClass PoolClassN(void)
{
  return EnsureNPoolClass();
}


Bool PoolNCheck(PoolN poolN)
{
  CHECKL(poolN != NULL);
  CHECKD(Pool, &poolN->poolStruct);
  CHECKL(poolN->poolStruct.class == EnsureNPoolClass());
  UNUSED(poolN); /* impl.c.mpm.check.unused */

  return TRUE;
}

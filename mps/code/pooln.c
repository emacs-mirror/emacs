/* impl.c.pooln: NULL POOL CLASS
 *
 * $HopeName: MMsrc!pooln.c(trunk.27) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 */

#include "pooln.h"
#include "mpm.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(trunk.27) $");


/* PoolNStruct -- the pool structure */

typedef struct PoolNStruct {
  PoolStruct poolStruct;                /* generic pool structure */
  /* and that's it */
} PoolNStruct;


/* PoolPoolN -- get the PoolN structure from generic Pool */

#define PoolPoolN(pool) PARENT(PoolNStruct, poolStruct, pool)


/* PoolPoolN -- get the generic pool structure from a PoolN */

#define PoolNPool(pooln) (&(poolN)->poolStruct)


/* NInit -- init method for class N */

static Res NInit(Pool pool, va_list args)
{
  PoolN poolN = PoolPoolN(pool);

  UNUSED(args);
  
  /* Initialize pool-specific structures. */

  AVERT(PoolN, poolN);
  EVENT_PPP(PoolInit, pool, PoolArena(pool), ClassOfPool(pool));
  return ResOK;
}


/* NFinish -- finish method for class N */

static void NFinish(Pool pool)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  /* Finish pool-specific structures. */
}


/* NAlloc -- alloc method for class N */

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


/* NFree -- free method for class N */

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


/* NBufferFill -- buffer fill method for class N */

static Res NBufferFill(Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size,
                       Bool withReservoirPermit)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  NOTREACHED;   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}


/* NBufferEmpty -- buffer empty method for class N */

static void NBufferEmpty(Pool pool, Buffer buffer, 
                         Addr init, Addr limit)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(init <= limit);

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
}


/* NDescribe -- describe method for class N */

static Res NDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(stream); /* @@@@ should output something here */

  return ResOK;
}


/* NWhiten -- condemn method for class N */

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


/* NGrey -- greyen method for class N */

static void NGrey(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
}


/* NBlacken -- blacken method for class N */

static void NBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
}


/* NScan -- scan method for class N */

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


/* NFix -- fix method for class N */

static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(ScanState, ss);
  UNUSED(refIO);
  AVERT(Seg, seg);
  NOTREACHED;  /* Since we don't allocate any objects, should never */
               /* be called upon to fix a reference. */
  return ResFAIL;
}


/* NReclaim -- reclaim method for class N */

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


/* NPoolClass -- pool class definition for N */

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
  this->bufferFill = NBufferFill;
  this->bufferEmpty = NBufferEmpty;
  this->whiten = NWhiten;
  this->grey = NGrey;
  this->blacken = NBlacken;
  this->scan = NScan;
  this->fix = NFix;
  this->fixEmergency = NFix;
  this->reclaim = NReclaim;
  this->describe = NDescribe;
}


/* PoolClassN -- returns the PoolClass for the null pool class */

PoolClass PoolClassN(void)
{
  return EnsureNPoolClass();
}


/* PoolNCheck -- check a pool of class N */

Bool PoolNCheck(PoolN poolN)
{
  CHECKL(poolN != NULL);
  CHECKD(Pool, &poolN->poolStruct);
  CHECKL(poolN->poolStruct.class == EnsureNPoolClass());
  UNUSED(poolN); /* impl.c.mpm.check.unused */

  return TRUE;
}

/* impl.c.pooln: NULL POOL
 *
 * $HopeName: MMsrc!pooln.c(MMdevel_bufferscan.2) $
 * Copyright(C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * This is the implementation of the null pool class.  Begin null it
 * all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(MMdevel_bufferscan.2) $");


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

static Res NAlloc(Addr *pReturn, Pool pool, Size size)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVER(pReturn != NULL);
  AVER(size > 0);

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

static Res NBufferInit(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(buffer);

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
                       Pool pool, Buffer buffer, Size size)
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

  NOTREACHED;   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static void NBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
}

static Res NDescribe(Pool pool, mps_lib_FILE *stream)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(stream);

  return ResOK;
}

static Res NCondemn(Pool pool, Trace trace, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Trace, trace);
  AVERT(Seg, seg);

  return ResOK;
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

static Res NScan(ScanState ss, Pool pool, Seg seg)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(ScanState, ss);
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

static PoolClassStruct PoolClassNStruct = {
  PoolClassSig,                         /* sig */
  "N",                                  /* name */
  sizeof(PoolNStruct),                  /* size */
  offsetof(PoolNStruct, poolStruct),    /* offset */
  AttrSCAN | AttrALLOC | AttrFREE | AttrBUF | AttrBUF_RESERVE | AttrGC,
  NInit,                                /* init */
  NFinish,                              /* finish */
  NAlloc,                               /* alloc */
  NFree,                                /* free */
  NBufferInit,                          /* bufferInit */
  NBufferFill,                          /* bufferFill */
  NBufferEmpty,                         /* bufferEmpty */
  NBufferFinish,                        /* bufferFinish */
  NCondemn,                             /* condemn */
  NGrey,                                /* grey */
  NScan,                                /* scan */
  NFix,                                 /* fix */
  NReclaim,                             /* reclaim */
  NDescribe,                            /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassN(void)
{
  return &PoolClassNStruct;
}

Bool PoolNCheck(PoolN poolN)
{
  CHECKL(poolN != NULL);
  CHECKD(Pool, &poolN->poolStruct);
  CHECKL(poolN->poolStruct.class == &PoolClassNStruct);
  return TRUE;
}


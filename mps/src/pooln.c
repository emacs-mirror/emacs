/*  impl.c.pooln
 *
 *                         NULL POOL
 *
 *  $HopeName: MMsrc!pooln.c(MMdevel_restr2.5) $
 *
 *  Copyright(C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the null pool class.  Begin null it
 *  all functions are implemented in a trivial manner.
 */

#include "mpm.h"
#include "pooln.h"

SRCID(pooln, "$HopeName: MMsrc!pooln.c(MMdevel_restr2.5) $");


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

  return ResLIMIT;  /* limit of nil buffers exceeded */
}

static void NBufferFinish(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Buffer, buffer);

  NOTREACHED;  /* can't create, so shouldn't destroy */
}

static Res NBufferFill(Addr *pReturn, Pool pool, Buffer buffer, Size size)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(pReturn != NULL);

  NOTREACHED;   /* can't create buffers, so shouldn't fill them */
  return ResUNIMPL;
}

static Bool NBufferTrip(Pool pool, Buffer buffer, Addr p, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(p != 0);
  AVER(size > 0);

  NOTREACHED;   /* can't create buffers, so they shouldn't trip */
  return FALSE;
}

static void NBufferExpose(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't expose them */
}

static void NBufferCover(Pool pool, Buffer buffer)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Buffer, buffer);

  NOTREACHED;   /* can't create buffers, so shouldn't cover them */
}

static Res NDescribe(Pool pool, Lib_FILE *stream)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  UNUSED(stream);

  return ResOK;
}

static Res NCondemn(RefSet *condemnedReturn, Pool pool,
                     Space space, TraceId ti)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVER(condemnedReturn != NULL);
  AVERT(Space, space);

  return ResOK;
}

static void NMark(Pool pool, Space space, TraceId ti)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Space, space);
}

static Res NScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVER(finishedReturn != NULL);
  AVERT(ScanState, ss);

  return ResOK;
}

static Res NFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ResFAIL;
}

static void NReclaim(Pool pool, Space space, TraceId ti)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Space, space);
  /* all unmarked and condemned objects reclaimed */
}

static void NAccess(Pool pool, Seg seg, AccessSet mode)
{
  PoolN poolN;

  AVERT(Pool, pool);
  poolN = PoolPoolN(pool);
  AVERT(PoolN, poolN);

  AVERT(Seg, seg);
  UNUSED(mode);
  /* deal with access to segment */
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
  NBufferFinish,                        /* bufferFinish */
  NBufferFill,                          /* bufferFill */
  NBufferTrip,                          /* bufferTrip */
  NBufferExpose,                        /* bufferExpose */
  NBufferCover,                         /* bufferCover */
  NCondemn,                             /* condemn */
  NMark,                                /* grey */
  NScan,                                /* scan */
  NFix,                                 /* fix */
  NReclaim,                             /* reclaim */
  NAccess,                              /* access */
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


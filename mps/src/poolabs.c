/* impl.c.poolabs: ABSTRACT POOL CLASSES
 *
 * $HopeName: MMsrc!poolabs.c(trunk.5) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: any MPS developer
 *
 * PURPOSE
 *
 * .purpose: This defines the abstract pool classes, giving
 * a single-inheritance framework which concrete classes 
 * may utilize.  The purpose is to reduce the fragility of class 
 * definitions for pool implementations when small changes are 
 * made to the pool protocol.   For now, the class hierarchy for
 * the abstract classes is intended to be useful, but not to
 * represent any particular design for pool inheritance.
 * 
 * HIERARCHY
 *
 * .hierarchy: define the following hierarchy of abstract pool classes:-
 *
 *    AbstractPoolClass     - implements init, finish, describe
 *     AbstractAllocFreePoolClass - implements alloc & free
 *     AbstractBufferPoolClass - implements the buffer protocol
 *      AbstractSegBufPoolClass - uses SegBuf buffer class 
 *       AbstractScanPoolClass - implements basic scanning
 *        AbstractCollectPoolClass - implements basic GC 
 */

#include "mpm.h"

SRCID(poolabs, "$HopeName: MMsrc!poolabs.c(trunk.5) $");

typedef PoolClassStruct AbstractPoolClassStruct;
typedef PoolClassStruct AbstractAllocFreePoolClassStruct;
typedef PoolClassStruct AbstractBufferPoolClassStruct;
typedef PoolClassStruct AbstractSegBufPoolClassStruct;
typedef PoolClassStruct AbstractScanPoolClassStruct;
typedef PoolClassStruct AbstractCollectPoolClassStruct;


/* Mixins:
 *
 * For now (at least) we're avoiding multiple inheritance. 
 * However, there is a significant use of multiple inheritance 
 * in practice amongst the pool classes, as there are several
 * orthogonal sub-protocols included in the pool protocol.
 * The following mixin functions help to provide the inheritance
 * via a simpler means than real multiple inheritance.
 */


/* PoolClassMixInAllocFree
 *
 * Mix in the protocol for Alloc / Free
 */

void PoolClassMixInAllocFree(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->attr |= (AttrALLOC | AttrFREE);
  class->alloc = PoolTrivAlloc;
  class->free = PoolTrivFree;
}


/* PoolClassMixInBuffer
 *
 * Mix in the protocol for buffer reserve / commit
 */

void PoolClassMixInBuffer(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->attr |= (AttrBUF | AttrBUF_RESERVE);
  class->bufferFill = PoolTrivBufferFill;
  class->bufferEmpty = PoolTrivBufferEmpty;
  /* By default, buffered pools treat frame operations as NOOPs */
  class->framePush = PoolTrivFramePush; 
  class->framePop = PoolTrivFramePop;
  class->bufferClass = EnsureBufferClass;
}


/* PoolClassMixInScan
 *
 * Mix in the protocol for scanning
 */

void PoolClassMixInScan(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->attr |= AttrSCAN;
  class->access = PoolSegAccess;
  class->blacken = PoolTrivBlacken;
  class->grey = PoolTrivGrey;
  /* Scan is part of the scanning  protocol - but there is */
  /* no useful default method */
  /*
  class->scan = PoolTrivScan;
  */
}


/* PoolClassMixInFormat
 *
 * Mix in the protocol for formatted pools
 */

void PoolClassMixInFormat(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->attr |= AttrFMT;
}


/* PoolClassMixInCollect
 *
 * Mix in the protocol for GC
 */

void PoolClassMixInCollect(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->attr |= (AttrGC | AttrINCR_RB);
  class->traceBegin = PoolTrivTraceBegin;
  class->whiten = PoolTrivWhiten;
  /* Fix, reclaim & benefit are part of the collection */
  /* protocol - but there are no useful default methods */
  /* for them */
  /*
  class->fix = PoolTrivFix;
  class->fixEmergency = PoolTrivFix;
  class->reclaim = PoolTrivReclaim;
  class->benefit = PoolTrivBenefit;
  */
  class->act = PoolCollectAct;
  class->rampBegin = PoolTrivRampBegin;
  class->rampEnd = PoolTrivRampEnd;
}


DEFINE_CLASS(AbstractPoolClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "ABSTRACT";
  class->size = 0;
  class->offset = 0;
  class->attr = 0;
  class->init = PoolTrivInit;
  class->finish = PoolTrivFinish;
  class->alloc = PoolNoAlloc;
  class->free = PoolNoFree;
  class->bufferFill = PoolNoBufferFill;
  class->bufferEmpty = PoolNoBufferEmpty;
  class->traceBegin = PoolNoTraceBegin;
  class->access = PoolNoAccess;
  class->whiten = PoolNoWhiten;
  class->grey = PoolNoGrey;
  class->blacken = PoolNoBlacken;
  class->scan = PoolNoScan;
  class->fix = PoolNoFix;
  class->fixEmergency = PoolNoFix;
  class->reclaim = PoolNoReclaim;
  class->benefit = PoolNoBenefit;
  class->act = PoolNoAct;
  class->rampBegin = PoolNoRampBegin;
  class->rampEnd = PoolNoRampEnd;
  class->framePush = PoolNoFramePush;
  class->framePop = PoolNoFramePop;
  class->framePopPending = PoolNoFramePopPending;
  class->walk = PoolNoWalk;
  class->bufferClass = PoolNoBufferClass;
  class->describe = PoolTrivDescribe;
  class->debugMixin = PoolNoDebugMixin;
  class->labelled = FALSE;
  class->sig = PoolClassSig;
}

DEFINE_CLASS(AbstractAllocFreePoolClass, class)
{
  INHERIT_CLASS(class, AbstractPoolClass);
  PoolClassMixInAllocFree(class);
}

DEFINE_CLASS(AbstractBufferPoolClass, class)
{
  INHERIT_CLASS(class, AbstractPoolClass);
  PoolClassMixInBuffer(class);
}

DEFINE_CLASS(AbstractSegBufPoolClass, class)
{
  INHERIT_CLASS(class, AbstractBufferPoolClass);
  class->bufferClass = EnsureSegBufClass;
}

DEFINE_CLASS(AbstractScanPoolClass, class)
{
  INHERIT_CLASS(class, AbstractSegBufPoolClass);
  PoolClassMixInScan(class);
}

DEFINE_CLASS(AbstractCollectPoolClass, class)
{
  INHERIT_CLASS(class, AbstractScanPoolClass);
  PoolClassMixInCollect(class);
}





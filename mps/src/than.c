/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(MMdevel_restr.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a single-threaded implementation of the threads manager.
 *  Has stubs for thread suspension.
 *  See design.mps.thread-manager.
 *
 *  .single: We only expect at most one thread on the ring.
 *
 *  This supports the impl.h.th
 */

#include "mpm.h"

SRCID(than, "$HopeName: MMsrc!than.c(MMdevel_restr.2) $");


Bool ThreadCheck(Thread thread)
{
  CHECKS(Thread, thread);
  CHECKU(Space, thread->space);
  CHECKL(thread->serial < thread->space->threadSerial);
  CHECKL(RingCheck(&thread->spaceRing));
  return TRUE;
}


Res ThreadRegister(Thread *threadReturn, Space space)
{
  Res res;
  Thread thread;
  Ring ring;

  AVER(threadReturn != NULL);

  res = SpaceAlloc((Addr *)&thread, space, sizeof(ThreadStruct));
  if(res != ResOK) return res;

  thread->space = space;
  RingInit(&thread->spaceRing);

  thread->sig = ThreadSig;
  thread->serial = space->threadSerial;
  ++space->threadSerial;

  AVERT(Thread, thread);

  ring = SpaceThreadRing(space);
  AVER(RingCheckSingle(ring));  /* .single */

  RingAppend(ring, &thread->spaceRing);

  *threadReturn = thread;
  return ResOK;
}

void ThreadDeregister(Thread thread, Space space)
{
  AVERT(Thread, thread);
  AVERT(Space, space);

  RingRemove(&thread->spaceRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->spaceRing);

  SpaceFree(space, (Addr)thread, sizeof(ThreadStruct));
}

void ThreadRingSuspend(Ring threadRing)
{
  AVERT(Ring, threadRing);
  return;
}

void ThreadRingResume(Ring threadRing)
{
  AVERT(Ring, threadRing);
  return;
}

/* Must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space ThreadSpace(Thread thread)
{
  return thread->space;
}

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  return StackScan(ss, stackBot);
}

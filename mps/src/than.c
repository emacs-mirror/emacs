/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(trunk.12) $
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

SRCID(than, "$HopeName: MMsrc!than.c(trunk.12) $");


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
  void *p;

  AVER(threadReturn != NULL);

  res = SpaceAlloc(&p, space, sizeof(ThreadStruct));
  if(res != ResOK) return res;
  thread = (Thread)p;

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

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Space ThreadSpace(Thread thread)
{
  return thread->space;
}

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  return StackScan(ss, stackBot);
}

Res ThreadDescribe(Thread thread, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Thread $P ($U) {\n", (void *)thread, (unsigned long)thread->serial,
               "  space $P ($U)\n",  (void *)thread->space, (unsigned long)thread->space->serial,
               "} Thread $P ($U)\n", (void *)thread, (unsigned long)thread->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}

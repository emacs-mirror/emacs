/*  impl.c.than: ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(trunk.19) $
 *  Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
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

SRCID(than, "$HopeName: MMsrc!than.c(trunk.19) $");


typedef struct ThreadStruct {   /* ANSI fake thread structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attaches to arena */
} ThreadStruct;


Bool ThreadCheck(Thread thread)
{
  CHECKS(Thread, thread);
  CHECKU(Arena, thread->arena);
  CHECKL(thread->serial < thread->arena->threadSerial);
  CHECKL(RingCheck(&thread->arenaRing));
  return TRUE;
}


Bool ThreadCheckSimple(Thread thread)
{
  CHECKS(Thread, thread);
  return TRUE;
}


Res ThreadRegister(Thread *threadReturn, Arena arena)
{
  Res res;
  Thread thread;
  Ring ring;
  void *p;

  AVER(threadReturn != NULL);

  res = ControlAlloc(&p, arena, sizeof(ThreadStruct), 
                     /* withReservoirPermit */ FALSE);
  if(res != ResOK) return res;
  thread = (Thread)p;

  thread->arena = arena;
  RingInit(&thread->arenaRing);

  thread->sig = ThreadSig;
  thread->serial = arena->threadSerial;
  ++arena->threadSerial;

  AVERT(Thread, thread);

  ring = ArenaThreadRing(arena);
  AVER(RingCheckSingle(ring));  /* .single */

  RingAppend(ring, &thread->arenaRing);

  *threadReturn = thread;
  return ResOK;
}


void ThreadDeregister(Thread thread, Arena arena)
{
  AVERT(Thread, thread);
  AVERT(Arena, arena);

  RingRemove(&thread->arenaRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->arenaRing);

  ControlFree(arena, thread, sizeof(ThreadStruct));
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

Thread ThreadRingThread(Ring threadRing)
{
  Thread thread;
  AVERT(Ring, threadRing);
  thread = RING_ELT(Thread, arenaRing, threadRing);
  AVERT(Thread, thread);
  return thread;
}


/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Arena ThreadArena(Thread thread)
{
  /* Can't AVER thread as that would not be thread-safe */
  /* AVERT(Thread, thread); */
  return thread->arena;
}


Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  UNUSED(thread);
  return StackScan(ss, stackBot);
}


Res ThreadDescribe(Thread thread, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Thread $P ($U) {\n", (WriteFP)thread, (WriteFU)thread->serial,
               "  arena $P ($U)\n",  
               (WriteFP)thread->arena, (WriteFU)thread->arena->serial,
               "} Thread $P ($U)\n", (WriteFP)thread, (WriteFU)thread->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}

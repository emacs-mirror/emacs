/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(trunk.8) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a single-threaded implementation of the threads manager.
 *  Has stubs for thread suspension.
 *  See design.mps.thread-manager.
 *
 *  .single: We only expect at most one thread on the deque.
 *
 *  This supports the impl.h.th
 */

#include "std.h"
#include "space.h"
#include "trace.h"
#include "ref.h"
#include "th.h"
#include "ss.h"

SRCID("$HopeName: MMsrc!than.c(trunk.8) $");

#define ThreadSig	((Sig)0x51924EAD)

typedef struct ThreadStruct
{
  Sig sig;
  DequeNodeStruct spaceDeque;  /* attaches to space */
} ThreadStruct;


#ifdef DEBUG

Bool ThreadIsValid(Thread thread, ValidationType validParam)
{
  AVER(thread->sig == ThreadSig);
  return TRUE;
}

#endif /* DEBUG */


Error ThreadRegister(Thread *threadReturn, Space space)
{
  Error e;
  Thread thread;
  Deque deque;

  AVER(threadReturn != NULL);

  e = PoolAlloc((Addr *)&thread, SpaceControlPool(space),
                sizeof(ThreadStruct));
  if(e != ErrSUCCESS)
    goto return_e;

  DequeNodeInit(&thread->spaceDeque);

  thread->sig = ThreadSig;

  AVER(ISVALID(Thread, thread));

  deque = SpaceThreadDeque(space);
  AVER(DequeIsEmpty(deque));  /* .single */

  DequeAppend(deque, &thread->spaceDeque);

  *threadReturn = thread;
  e = ErrSUCCESS;
return_e:
  /* single exit point.  Will release lock here. */
  return e;
}

void ThreadDeregister(Thread thread, Space space)
{
  AVER(ISVALID(Thread, thread));
  AVER(ISVALID(Space, space));

  DequeNodeRemove(&thread->spaceDeque);

  thread->sig = SigInvalid;

  DequeNodeFinish(&thread->spaceDeque);

  PoolFree(SpaceControlPool(space), (Addr)thread, sizeof(ThreadStruct));
}

void ThreadDequeSuspend(Deque threadDeque)
{
  AVER(ISVALID(Deque, threadDeque));
  return;
}

void ThreadDequeResume(Deque threadDeque)
{
  AVER(ISVALID(Deque, threadDeque));
  return;
}

/* Must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space ThreadSpace(Thread thread)
{
  return PARENT(SpaceStruct, threadDeque, thread->spaceDeque.deque);
}

Error ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  return StackScan(ss, stackBot);
}

/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(trunk.4) $
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
#include "trace.h"
#include "ref.h"
#include "space.h"
#include "th.h"
#include "ss.h"


typedef struct ThreadStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  DequeNodeStruct spaceDeque;  /* attaches to space */
} ThreadStruct;


#ifdef DEBUG_SIGN
static SigStruct ThreadSigStruct;
#endif

#ifdef DEBUG_ASSERT

Bool ThreadIsValid(Thread thread, ValidationType validParam)
{
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &ThreadSigStruct));
  AVER(thread->sig == &ThreadSigStruct);
#endif
  return TRUE;
}

#endif /* DEBUG_ASSERT */


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

#ifdef DEBUG_SIGN
  SigInit(&ThreadSigStruct, "Thread");
  thread->sig = &ThreadSigStruct;
#endif

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

#ifdef DEBUG_SIGN
  thread->sig = SigInvalid;
#endif

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


/* thread safe */
Space ThreadSpace(Thread thread)
{
  return PARENT(SpaceStruct, threadDeque, &thread->spaceDeque);
}

Error ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  return StackScan(ss, stackBot);
}

/*  impl.c.than
 *
 *                  ANSI THREADS MANAGER
 *
 *  $HopeName: MMsrc!than.c(trunk.3) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a single-threaded implementation of the threads manager.
 *  It implements stack scanning, and has stubs for thread suspension.
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
  Addr *stackBot;   /* The bottom (most hidden) word of stack. */
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
  AVER(thread->stackBot != NULL);
  return TRUE;
}

#endif /* DEBUG_ASSERT */


Error ThreadRegister(Thread *threadReturn, Space space, Addr *stackBot)
{
  Error e;
  Thread thread;
  Deque deque;

  AVER(threadReturn != NULL);
  AVER(stackBot != NULL);

  e = PoolAlloc((Addr *)&thread, SpaceControlPool(space),
                sizeof(ThreadStruct));
  if(e != ErrSUCCESS)
    goto return_e;

  thread->stackBot = stackBot;
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


/*  Scan this thread's stack, if the thread is registered (.single) */
/*  p should be a deque containing the thread */

Error ThreadDequeScan(void *p, int i, Trace trace)
{
  Deque deque;   /* of threads */
  DequeNode node;
  Thread thread;
  Error e;

  UNUSED(i);

  deque = (Deque)p;
  AVER(ISVALID(Deque, deque));

  if(DequeIsEmpty(deque))  /* May have no registered threads */
    return ErrSUCCESS;

  node = DequeFirst(deque);
  thread = DEQUENODEELEMENT(Thread, spaceDeque, node);
  AVER(ISVALID(Thread, thread));

  /* at most one thread (.single) */
  AVER(DequeNodeNext(node) == DequeSentinel(deque)); 

  e = StackScan(thread->stackBot, trace, RefRankAMBIG);

  return e;
}

/* thread safe */
Space ThreadSpace(Thread thread)
{
  return PARENT(SpaceStruct, threadDeque, &thread->spaceDeque);
}

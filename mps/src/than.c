/*  impl.c.than
 *                    ANSI THREADS
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a single-threaded implementation of the threads module.  It
 *  implements trampolining, stack scanning, and has stubs for thread
 *  suspension.
 *
 *  This is intended to be used in conjunction with impl.h.than
 *  and any other relevent files to provide an implementation in
 *  support of impl.h.th.
 *  In particular it is likely that each architecture will need 
 *  its own implementation of ThreadScanStack.
 */

#include "std.h"
#include "trace.h"
#include "ref.h"
#include "space.h"
#include "th.h"
#include "than.h"


typedef struct ThreadStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  Addr *stackBot;   /* The bottom (most hidden) word of stack. */
} ThreadStruct;

typedef struct ThreadsStruct
{
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  struct ThreadStruct threadStruct;   /* The single thread. */
} ThreadsStruct;


#ifdef DEBUG_SIGN
static SigStruct ThreadsSigStruct;
static SigStruct ThreadSigStruct;
#endif

#ifdef DEBUG_ASSERT
Bool ThreadsIsValid(Threads threads, ValidationType validParam)
{
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &ThreadsSigStruct));
  AVER(threads->sig == &ThreadsSigStruct);
#endif
  return TRUE;
}

Bool ThreadIsValid(Thread thread, ValidationType validParam)
{
  AVER(thread->stackBot !=NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &ThreadSigStruct));
  AVER(thread->sig == &ThreadSigStruct);
#endif
  AVER(thread->stackBot != NULL);
  return TRUE;
}
#endif /* DEBUG_ASSERT */

Error ThreadsCreate(Threads *threadsReturn, Space space)
{
  Error e;
  Threads threads;

  e = PoolAllocP((void **)&threads, SpaceControlPool(space),
                 sizeof(ThreadsStruct));
  if(e != ErrSUCCESS)
    return e;

#ifdef DEBUG_SIGN
  SigInit(&ThreadsSigStruct, "Threads");
  threads->sig = &ThreadsSigStruct;
#endif
  AVER(ISVALID(Threads, threads));
  *threadsReturn = threads;
  return ErrSUCCESS;
}

void ThreadsDestroy(Threads threads, Space space)
{
  AVER(ISVALID(Threads, threads));
#ifdef DEBUG_SIGN
  threads->sig = SigInvalid;
#endif
}

Error ThreadRegister(Thread *threadReturn, Addr *stackBot, Space space)
{
  Threads threads;
  Thread thread;

  AVER(threadReturn != NULL);
  AVER(stackBot != NULL);

  threads = SpaceThreads(space);
  thread = &threads->threadStruct;

  thread->stackBot = stackBot;
#ifdef DEBUG_SIGN
  SigInit(&ThreadSigStruct, "Thread");
  thread->sig = &ThreadSigStruct;
#endif

  AVER(ISVALID(Thread, thread));
  *threadReturn = thread;
  return ErrSUCCESS;
}

extern void ThreadDeregister(Thread thread, Space space)
{
  Threads threads;

  AVER(ISVALID(Thread, thread));
  threads = SpaceThreads(space);

  AVER(&threads->threadStruct == thread);

#ifdef DEBUG_SIGN
  thread->sig = SigInvalid;
#endif
  thread->stackBot = NULL;
}


void (ThreadTrampoline)(void **valueReturn,
                        Thread thread,
                        ThreadContinue function,
                        void *arg)
{
  AVER(valueReturn != NULL);
  AVER(ISVALID(Thread, thread));
  AVER(function != NULL);

  *valueReturn = function(arg);
}

void ThreadsSuspend(Threads threads)
{
  AVER(ISVALID(Threads, threads));
  return;
}

void ThreadsResume(Threads threads)
{
  AVER(ISVALID(Threads, threads));
  return;
}

Error ThreadsScanStacks(void *p, int i, Trace trace)
{ /* scan this threads stack, which is the only thread */
  Threads threads;
  Thread thread;
  Error e;

  threads = (Threads)p;
  AVER(ISVALID(Threads, threads));

  thread = &threads->threadStruct;
  AVER(ISVALID(Thread, thread));

  /* call machine specific routine */
  e = ThreadScanStack(thread->stackBot, trace, RefRankAMBIG);

  return e;
}

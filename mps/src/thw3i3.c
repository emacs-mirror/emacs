/*  impl.c.thnti3
 * 
 *                  WIN32 THREAD MANAGER
 *
 *  $HopeName: MMsrc/!thnti3.c(trunk.1)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Implements thread registration, suspension, and stack
 *  scanning.  See design.mps.thread-manager
 *
 *  This supports the impl.h.th
 *
 *  .thread.id: The thread id is used to identify the current thread.
 *  .thread.handle: The thread handle needs the enough access to
 *  be able to suspend threads and to get their context.  i.e.
 *  .thread.handle.susp-res: THREAD_SUSPEND_RESUME access
 *  .thread.handle.get-context: THREAD_GET_CONTEXT access
 *  An appropriate handle is created on registration.
 *
 *  ASSUMPTIONS
 *
 *  .error: some major errors are assumed not to happen.
 *  .error.close-handle: CloseHandle is assumed to succeed.
 *  .error.resume: ResumeThread is assumed to succeed.
 *  .error.suspend: SuspendThread is assumed to succeed.
 *  .error.get-context: GetThreadContext is assumed to succeed
 *
 *  .stack.full-descend:  assumes full descending stack.
 *  i.e. stack pointer points to the last allocated location;
 *  stack grows downwards.
 *  
 *  .i3: assumes ARCH_386
 *  .i3.sp: The sp in the context is Esp
 *  .i3.context: Esp is in control context so .context.sp holds
 *  The root registers are Edi, Esi, Ebx, Edx, Ecx, Eax 
 *  these are given by CONTEXT_INTEGER, so .context.regroots holds.
 *
 *  .nt: uses Win32 specific stuff
 *  HANDLE
 *  DWORD
 *  GetCurrentProcess
 *  DuplicateHandle
 *  THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT
 *  GetCurrentThreadId
 *  CloseHandle
 *  SuspendThread
 *  ResumeThread
 *  CONTEXT
 *  CONTEXT_CONTROL | CONTEXT_INTEGER
 *  GetThreadContext
 *
 *  .context: ContextFlags determine what is recorded by 
 *  GetThreadContext.  This should be set to whichever bits of the
 *  context that need to be recorded.  This should include:
 *  .context.sp: sp assumed to be recorded by CONTEXT_CONTROL.
 *  .context.regroots: assumed to be recorded by CONTEXT_INTEGER.
 *  see winnt.h for description of CONTEXT and ContextFlags.
 *
 */

#if !defined(OS_NT) || !defined(ARCH_386) /* .i3 .nt */
#error "Compiling thntmvi3 when OS_NT or ARCH_386 not defined."
#endif

#include "std.h"
#include "deque.h"
#include "trace.h"
#include "ref.h"
#include "pool.h"
#include "space.h"
#include "th.h"
#include "ss.h"


#include "windows.h"

typedef struct ThreadStruct {
#ifdef DEBUG_SIGN
  Sig sig;
#endif
  DequeNodeStruct spaceDeque; /* threads attached to space */
  HANDLE handle;   /* Handle of thread .thread.handle */
  DWORD id;        /* Thread id of thread */
  Addr *stackBot;  /* bottom of stack i.e. most hidden */
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

  AVER(ISVALIDNESTED(DequeNode, &thread->spaceDeque));
  AVER(thread->stackBot != NULL);

  return TRUE;
}

#endif /* DEBUG_ASSERT */


Error ThreadRegister(Thread *threadReturn, Space space, Addr *stackBot)
{
  Error e;
  Thread thread;
  HANDLE procHandle;
  BOOL b;

  AVER(threadReturn != NULL);
  AVER(stackBot != NULL);
  AVER(ISVALID(Space, space));

  e = PoolAlloc((Addr *)&thread, SpaceControlPool(space),
    sizeof(ThreadStruct));
  if(e != ErrSUCCESS)
    goto return_e;

  /* Duplicate handle gives us a new handle with updated privileges.
   * .thread.handle describes the ones needed.
   */
  procHandle = GetCurrentProcess();

  b = DuplicateHandle(procHandle, GetCurrentThread(), procHandle,
                       &thread->handle, 
                       THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT,
                       FALSE, 0);
  if(!b){
    e = ErrRESOURCE;
    goto return_e;
  }

  thread->id = GetCurrentThreadId();
  thread->stackBot = stackBot;

  DequeNodeInit(&thread->spaceDeque);

#ifdef DEBUG_SIGN
  SigInit(&ThreadSigStruct, "Thread");
  thread->sig = &ThreadSigStruct;
#endif

  AVER(ISVALID(Thread, thread));

  DequeAppend(SpaceThreadDeque(space), &thread->spaceDeque);

  *threadReturn = thread;
  e = ErrSUCCESS;
return_e:
  /* Common exit point: will release lock here */
  return e;
}

void ThreadDeregister(Thread thread, Space space)
{
  Bool b;
  
  AVER(ISVALID(Thread, thread));
  AVER(ISVALID(Space, space));

  DequeNodeRemove(&thread->spaceDeque);

#ifdef DEBUG_SIGN
  thread->sig = SigInvalid;
#endif

  DequeNodeFinish(&thread->spaceDeque);

  b = CloseHandle(thread->handle);
  AVER(b); /* .error.close-handle */

  PoolFree(SpaceControlPool(space), (Addr)thread, sizeof(ThreadStruct));
}


/*  Map over threads on deque calling f on each one except the
 *  current thread.
 */
static void mapThreadDeque(Deque deque, void (*f)(Thread thread))
{
  DequeNode node;
  DWORD id;

  id = GetCurrentThreadId();
  node = DequeFirst(deque);
  while(node != DequeSentinel(deque)) {
    DequeNode next = DequeNodeNext(node);
    Thread thread;

    thread = DEQUENODEELEMENT(Thread, spaceDeque, node);
    AVER(ISVALID(Thread, thread));
    if(id != thread->id) /* .thread.id */
      (*f)(thread);

    node = next;
  }
}

static void suspend(Thread thread)
{ 
  DWORD c;
  /* .thread.handle.susp-res */
  c = SuspendThread(thread->handle);
  AVER(0xFFFFFFFF != c); /* .error.suspend */
}

void ThreadDequeSuspend(Deque deque)
{
  mapThreadDeque(deque, suspend);
}

static void resume(Thread thread)
{
  DWORD c;
  /* .thread.handle.susp-res */
  c = ResumeThread(thread->handle);
  AVER(0xFFFFFFFF != c); /* .error.resume */
}

void ThreadDequeResume(Deque deque)
{
  mapThreadDeque(deque, resume);
}

Error ThreadDequeScan(void *p, int i, Trace trace)
{
  Deque deque;
  DequeNode node;
  DWORD id;
  Error e;

  UNUSED(i);

  deque = (Deque)p;

  AVER(ISVALID(Deque, deque));

  id = GetCurrentThreadId();
  node = DequeFirst(deque);
  while(node != DequeSentinel(deque)) {
    DequeNode next = DequeNodeNext(node);
    Thread thread;

    thread = DEQUENODEELEMENT(Thread, spaceDeque, node);
    if(id != thread->id) { /* .thread.id */
      CONTEXT context;
      BOOL success;
        
      /* scan stack and register roots in other threads */

      /* This dumps the relevent registers into the context */
      /* .context.flags */
      context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
      /* .thread.handle.get-context */
      success = GetThreadContext(thread->handle, &context);
      AVER(success); /* .error.get-context */
 
      /* scan stack inclusive of current sp and exclusive of
       * stackBot (.stack.full-descend)
       */
      e = TraceScanArea(((Addr *)context.Esp), /* .i3.sp */
                          thread->stackBot, trace, RefRankAMBIG);
      if(e != ErrSUCCESS)
        return e;

      /* (.context.regroots)
       * This scans the root registers (.context.regroots).  It also
       * unecessarily scans the rest of the context.  The optimisation
       * to scan only relevent parts would be machine dependent.
       */
      e = TraceScanArea((Addr *)&context,
             (Addr *)((char *)&context + sizeof(CONTEXT)),
             trace, RefRankAMBIG);
      if(e != ErrSUCCESS)
        return e;

    } else { /* scan this thread's stack */
      e = StackScan(thread->stackBot, trace, RefRankAMBIG);
      if(e != ErrSUCCESS)
        return e;
    }
    node = next;
  }

  return ErrSUCCESS;
}

/*  impl.c.thnti3
 *
 *                  WIN32 THREAD MANAGER
 *
 *  $HopeName: MMsrc!thnti3.c(MMdevel_restr.3) $
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
 *  .i3: assumes MPS_ARCH_I3
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

#include "mpm.h"

#if !defined(MPS_OS_W3) || !defined(MPS_ARCH_I3) /* .i3 .nt */
#error "Compiling thnti3 when MPS_OS_W3 or MPS_ARCH_I3 not defined."
#endif

#include <windows.h>

SRCID(thnti3, "$HopeName: MMsrc!thnti3.c(MMdevel_restr.3) $");

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
  HANDLE procHandle;
  BOOL b;

  AVER(threadReturn != NULL);
  AVERT(Space, space);

  res = SpaceAlloc((Addr *)&thread, space, sizeof(ThreadStruct));
  if(res != ResOK)
    return res;

  /* Duplicate handle gives us a new handle with updated privileges.
   * .thread.handle describes the ones needed.
   */
  procHandle = GetCurrentProcess();

  b = DuplicateHandle(procHandle, GetCurrentThread(), procHandle,
                       &thread->handle,
                       THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT,
                       FALSE, 0);
  if(!b)
    return ResRESOURCE;

  thread->id = GetCurrentThreadId();

  RingInit(&thread->spaceRing);

  thread->sig = ThreadSig;
  thread->serial = space->threadSerial;
  ++space->threadSerial;
  thread->space = space;

  AVERT(Thread, thread);

  RingAppend(SpaceThreadRing(space), &thread->spaceRing);

  *threadReturn = thread;
  return ResOK;
}

void ThreadDeregister(Thread thread, Space space)
{
  Bool b;

  AVERT(Thread, thread);
  AVERT(Space, space);

  RingRemove(&thread->spaceRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->spaceRing);

  b = CloseHandle(thread->handle);
  AVER(b); /* .error.close-handle */

  SpaceFree(space, (Addr)thread, sizeof(ThreadStruct));
}


/*  Map over threads on ring calling f on each one except the
 *  current thread.
 */
static void mapThreadRing(Ring ring, void (*f)(Thread thread))
{
  Ring node;
  DWORD id;

  id = GetCurrentThreadId();
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Thread thread;

    thread = RING_ELT(Thread, spaceRing, node);
    AVERT(Thread, thread);
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

void ThreadRingSuspend(Ring ring)
{
  mapThreadRing(ring, suspend);
}

static void resume(Thread thread)
{
  DWORD c;
  /* .thread.handle.susp-res */
  c = ResumeThread(thread->handle);
  AVER(0xFFFFFFFF != c); /* .error.resume */
}

void ThreadRingResume(Ring ring)
{
  mapThreadRing(ring, resume);
}

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  DWORD id;
  Res res;

  id = GetCurrentThreadId();

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
    res = TraceScanAreaTagged(ss, ((Addr *)context.Esp), /* .i3.sp */
                        stackBot);
    if(res != ResOK)
      return res;

    /* (.context.regroots)
     * This scans the root registers (.context.regroots).  It also
     * unecessarily scans the rest of the context.  The optimisation
     * to scan only relevent parts would be machine dependent.
     */
    res = TraceScanAreaTagged(ss, (Addr *)&context,
           (Addr *)((char *)&context + sizeof(CONTEXT)));
    if(res != ResOK)
      return res;

  } else { /* scan this thread's stack */
    res = StackScan(ss, stackBot);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}

/* Must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space ThreadSpace(Thread thread)
{
  return thread->space;
}

/* impl.c.thw3i3: WIN32 THREAD MANAGER
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * Implements thread registration, suspension, and stack
 * scanning.  See design.mps.thread-manager
 *
 * This supports the impl.h.th
 *
 * .thread.id: The thread id is used to identify the current thread.
 * .thread.handle: The thread handle needs the enough access to
 * be able to suspend threads and to get their context.  i.e.
 * .thread.handle.susp-res: THREAD_SUSPEND_RESUME access
 * .thread.handle.get-context: THREAD_GET_CONTEXT access
 * An appropriate handle is created on registration.
 *
 *
 * ASSUMPTIONS
 *
 * .error: some major errors are assumed not to happen.
 * .error.close-handle: CloseHandle is assumed to succeed.
 *
 * Other errors are assumed to only happen in certain circumstances.
 * .error.resume: ResumeThread is assumed to succeed unless the thread
 * has been destroyed (in fact, perversely, it appears to succeeed even
 * when the thread has been destroyed).
 * .error.suspend: SuspendThread is assumed to succeed unless the thread
 * has been destroyed.
 * .error.get-context: GetThreadContext is assumed to succeed unless the
 * thread has been destroyed.
 *
 * .stack.full-descend:  assumes full descending stack.
 * i.e. stack pointer points to the last allocated location;
 * stack grows downwards.
 *
 * .stack.below-bottom: it's legal for the stack pointer to be at a
 * higher address than the registered bottom of stack. This might
 * happen if the stack of another thread doesn't contain any frames
 * belonging to the client language. In this case, the stack should
 * not be scanned.
 *
 * .stack.align: assume roots on the stack are always word-aligned,
 * but don't assume that the stack pointer is necessarily
 * word-aligned at the time of reading the context of another thread.
 *
 * .i3: assumes MPS_ARCH_I3
 * .i3.sp: The sp in the context is Esp
 * .i3.context: Esp is in control context so .context.sp holds
 * The root registers are Edi, Esi, Ebx, Edx, Ecx, Eax
 * these are given by CONTEXT_INTEGER, so .context.regroots holds.
 *
 * .nt: uses Win32 specific stuff
 * HANDLE
 * DWORD
 * GetCurrentProcess
 * DuplicateHandle
 * THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT
 * GetCurrentThreadId
 * CloseHandle
 * SuspendThread
 * ResumeThread
 * CONTEXT
 * CONTEXT_CONTROL | CONTEXT_INTEGER
 * GetThreadContext
 *
 * .context: ContextFlags determine what is recorded by
 * GetThreadContext.  This should be set to whichever bits of the
 * context that need to be recorded.  This should include:
 * .context.sp: sp assumed to be recorded by CONTEXT_CONTROL.
 * .context.regroots: assumed to be recorded by CONTEXT_INTEGER.
 * see winnt.h for description of CONTEXT and ContextFlags.
 */

#include "mpm.h"

#if !defined(MPS_OS_W3) || !defined(MPS_ARCH_I3) /* .i3 .nt */
#error "Compiling thnti3 when MPS_OS_W3 or MPS_ARCH_I3 not defined."
#endif

#include "mpswin.h"

SRCID(thw3i3, "$Id$");


typedef struct ThreadStruct {   /* Win32 thread structure */
  Sig sig;                      /* design.mps.sig */
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* threads attached to arena */
  HANDLE handle;                /* Handle of thread, see
                                 * impl.c.thnti3.thread.handle */
  DWORD id;                     /* Thread id of thread */
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
  HANDLE procHandle;
  BOOL b;
  void *p;

  AVER(threadReturn != NULL);
  AVERT(Arena, arena);

  res = ControlAlloc(&p, arena, sizeof(ThreadStruct),
                     /* withReservoirPermit */ FALSE);
  if(res != ResOK)
    return res;
  thread = (Thread)p; /* avoid pun */

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

  RingInit(&thread->arenaRing);

  thread->sig = ThreadSig;
  thread->serial = arena->threadSerial;
  ++arena->threadSerial;
  thread->arena = arena;

  AVERT(Thread, thread);

  RingAppend(ArenaThreadRing(arena), &thread->arenaRing);

  *threadReturn = thread;
  return ResOK;
}

void ThreadDeregister(Thread thread, Arena arena)
{
  Bool b;

  AVERT(Thread, thread);
  AVERT(Arena, arena);

  RingRemove(&thread->arenaRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->arenaRing);

  b = CloseHandle(thread->handle);
  AVER(b); /* .error.close-handle */

  ControlFree(arena, thread, sizeof(ThreadStruct));
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

    thread = RING_ELT(Thread, arenaRing, node);
    AVERT(Thread, thread);
    if(id != thread->id) /* .thread.id */
      (*f)(thread);

    node = next;
  }
}

static void suspend(Thread thread)
{
  /* .thread.handle.susp-res */
  /* .error.suspend */
  /* In the error case (SuspendThread returning 0xFFFFFFFF), we */
  /* assume the thread has been destroyed (as part of process shutdown). */
  /* In which case we simply continue. */
  /* [GetLastError appears to return 5 when SuspendThread is called */
  /* on a destroyed thread, but I'm not sufficiently confident of this */
  /* to check -- drj 1998-04-09] */
  (void)SuspendThread(thread->handle);
}

void ThreadRingSuspend(Ring ring)
{
  mapThreadRing(ring, suspend);
}

static void resume(Thread thread)
{
  /* .thread.handle.susp-res */
  /* .error.resume */
  /* In the error case (ResumeThread returning 0xFFFFFFFF), we */
  /* assume the thread has been destroyed (as part of process shutdown). */
  /* In which case we simply continue. */
  (void)ResumeThread(thread->handle);
}

void ThreadRingResume(Ring ring)
{
  mapThreadRing(ring, resume);
}


Thread ThreadRingThread(Ring threadRing)
{
  Thread thread;
  AVERT(Ring, threadRing);
  thread = RING_ELT(Thread, arenaRing, threadRing);
  AVERT(Thread, thread);
  return thread;
}


Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  DWORD id;
  Res res;

  id = GetCurrentThreadId();

  if(id != thread->id) { /* .thread.id */
    CONTEXT context;
    BOOL success;
    Addr *stackBase, *stackLimit, stackPtr;

    /* scan stack and register roots in other threads */

    /* This dumps the relevent registers into the context */
    /* .context.flags */
    context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
    /* .thread.handle.get-context */
    success = GetThreadContext(thread->handle, &context);
    if(!success) {
      /* .error.get-context */
      /* We assume that the thread must have been destroyed. */
      /* We ignore the situation by returning immediately. */
      return ResOK;
    }

    stackPtr  = (Addr)context.Esp;   /* .i3.sp */
    /* .stack.align */
    stackBase  = (Addr *)AddrAlignUp(stackPtr, sizeof(Addr));
    stackLimit = (Addr *)stackBot;
    if (stackBase >= stackLimit)
      return ResOK;    /* .stack.below-bottom */

    /* scan stack inclusive of current sp and exclusive of
     * stackBot (.stack.full-descend)
     */
    res = TraceScanAreaTagged(ss, stackBase, stackLimit);
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

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Arena ThreadArena(Thread thread)
{
  /* Can't AVER thread as that would not be thread-safe */
  /* AVERT(Thread, thread); */
  return thread->arena;
}

Res ThreadDescribe(Thread thread, mps_lib_FILE *stream)
{
  Res res;
 
  res = WriteF(stream,
               "Thread $P ($U) {\n", (WriteFP)thread, (WriteFU)thread->serial,
               "  arena $P ($U)\n", 
               (WriteFP)thread->arena, (WriteFU)thread->arena->serial,
               "  handle $W\n",      (WriteFW)thread->handle,
               "  id $U\n",          (WriteFU)thread->id,
               "} Thread $P ($U)\n", (WriteFP)thread, (WriteFU)thread->serial,
               NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* thw3i3.c: WIN32 THREAD MANAGER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * Implements thread registration, suspension, and stack
 * scanning.  See <design/thread-manager/>.
 *
 * This supports the <code/th.h> along with <code/thw3i3.c> or <code/thw3i6.c>
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
 * has been destroyed (in fact, perversely, it appears to succeed even
 * when the thread has been destroyed).
 * .error.suspend: SuspendThread is assumed to succeed unless the thread
 * has been destroyed.
 *
 *
 * .nt: uses Win32 specific stuff
 * HANDLE
 * DWORD
 * GetCurrentProcess
 * DuplicateHandle
 * THREAD_SUSPEND_RESUME
 * GetCurrentThreadId
 * CloseHandle
 * SuspendThread
 * ResumeThread
 *
 */

#include "mpm.h"

#if !defined(MPS_OS_W3) /* .nt */
#error "Compiling thw3 when MPS_OS_W3 not defined."
#endif

#include "thw3.h"

#include "mpswin.h"

SRCID(thw3, "$Id$");


Bool ThreadCheck(Thread thread)
{
  CHECKS(Thread, thread);
  CHECKU(Arena, thread->arena);
  CHECKL(thread->serial < thread->arena->threadSerial);
  CHECKD_NOSIG(Ring, &thread->arenaRing);
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

/* Must be thread-safe.  See <design/interface-c/#thread-safety>. */
Arena ThreadArena(Thread thread)
{
  /* Can't AVER thread as that would not be thread-safe */
  /* AVERT(Thread, thread); */
  return thread->arena;
}

Res ThreadDescribe(Thread thread, mps_lib_FILE *stream, Count depth)
{
  Res res;
 
  res = WriteF(stream, depth,
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
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

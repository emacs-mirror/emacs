/* thxc.c: OS X MACH THREADS MANAGER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/thread-manager/>.
 *
 *
 * TODO
 *
 * Too much code in common with than.c, thix.c.  Consider how to reduce
 * redundancy without making the code obscure.
 *
 *
 * REFERENCES
 *
 * [Mach_man]  Mach man pages within XNU;
 *             Apple Computer;
 *             <http://www.opensource.apple.com/source/xnu/xnu-2050.22.13/osfmk/man/>.
 */

#include "mpm.h"
#include "protxc.h"

#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/thread_act.h>
#include <mach/thread_status.h>


SRCID(thxc, "$Id$");


typedef struct mps_thr_s {      /* OS X / Mach thread structure */
  Sig sig;                      /* <design/sig/> */
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attaches to arena */
  thread_port_t port;           /* thread kernel port */
} ThreadStruct;


Bool ThreadCheck(Thread thread)
{
  CHECKS(Thread, thread);
  CHECKU(Arena, thread->arena);
  CHECKL(thread->serial < thread->arena->threadSerial);
  CHECKD_NOSIG(Ring, &thread->arenaRing);
  CHECKL(MACH_PORT_VALID(thread->port));
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

  thread->serial = arena->threadSerial;
  ++arena->threadSerial;
  thread->port = mach_thread_self();
  thread->sig = ThreadSig;
  AVERT(Thread, thread);

  ProtThreadRegister(FALSE);

  ring = ArenaThreadRing(arena);

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


/*  mapThreadRing -- map over threads on ring calling a function on each one
 *                   except the current thread
 */

static void mapThreadRing(Ring threadRing, void (*func)(Thread))
{
  Ring node, next;
  mach_port_t self;

  AVERT(Ring, threadRing);

  self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  RING_FOR(node, threadRing, next) {
    Thread thread = RING_ELT(Thread, arenaRing, node);
    AVERT(Thread, thread);
    if(thread->port != self)
      (*func)(thread);
  }
}


static void threadSuspend(Thread thread)
{
  kern_return_t kern_return;
  kern_return = thread_suspend(thread->port);
  /* No rendezvous is necessary: thread_suspend "prevents the thread
   * from executing any more user-level instructions" */
  AVER(kern_return == KERN_SUCCESS);
}

static void threadResume(Thread thread)
{
  kern_return_t kern_return;
  kern_return = thread_resume(thread->port);
  /* Mach has no equivalent of EAGAIN. */
  AVER(kern_return == KERN_SUCCESS);
}


/* ThreadRingSuspend -- suspend all threads on a ring, except the
 * current one.
 */
void ThreadRingSuspend(Ring threadRing)
{
  AVERT(Ring, threadRing);
  mapThreadRing(threadRing, threadSuspend);
}

/* ThreadRingResume -- resume all threads on a ring, except the
 * current one.
 */
void ThreadRingResume(Ring threadRing)
{
  AVERT(Ring, threadRing);
  mapThreadRing(threadRing, threadResume);
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


/* ThreadScan -- scan the state of a thread (stack and regs) */

#include "prmcxc.h"

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  mach_port_t self;
  Res res;

  AVERT(Thread, thread);
  self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  if (thread->port == self) {
    /* scan this thread's stack */
    res = StackScan(ss, stackBot);
    if(res != ResOK)
      return res;
  } else {
    MutatorFaultContextStruct mfcStruct;
    THREAD_STATE_S threadState;
    Addr *stackBase, *stackLimit, stackPtr;
    mach_msg_type_number_t count;
    kern_return_t kern_return;

    /* Note: We could get the thread state and check the suspend cound in
       order to assert that the thread is suspended, but it's probably
       unnecessary and is a lot of work to check a static condition. */

    mfcStruct.address = NULL;
    mfcStruct.threadState = &threadState;

    count = THREAD_STATE_COUNT;
    AVER(sizeof(*mfcStruct.threadState) == count * sizeof(natural_t));
    kern_return = thread_get_state(thread->port,
                                   THREAD_STATE_FLAVOR,
                                   (thread_state_t)mfcStruct.threadState,
                                   &count);
    AVER(kern_return == KERN_SUCCESS);
    AVER(count == THREAD_STATE_COUNT);

    stackPtr = MutatorFaultContextSP(&mfcStruct);
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

    /* scan the registers in the mutator fault context */
    res = MutatorFaultContextScan(ss, &mfcStruct);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}


Res ThreadDescribe(Thread thread, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "Thread $P ($U) {\n", (WriteFP)thread, (WriteFU)thread->serial,
               "arena $P ($U)\n",
               (WriteFP)thread->arena, (WriteFU)thread->arena->serial,
               "port $U\n", (WriteFU)thread->port,
               "} Thread $P ($U)\n", (WriteFP)thread, (WriteFU)thread->serial,
               NULL);
  if(res != ResOK) return res;

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

/* thxc.c: THREAD MANAGER (macOS)
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
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

#if !defined(MPS_OS_XC)
#error "protw3.c is specific to MPS_OS_XC"
#endif

#include "protxc.h"

#include <mach/mach_init.h>
#include <mach/task.h>
#include <mach/thread_act.h>
#include <mach/thread_status.h>
#include <pthread.h>


SRCID(thxc, "$Id$");


typedef struct mps_thr_s {      /* macOS thread structure */
  Sig sig;                      /* <design/sig/> */
  Serial serial;                /* from arena->threadSerial */
  Arena arena;                  /* owning arena */
  RingStruct arenaRing;         /* attaches to arena */
  Bool alive;                   /* thread believed to be alive? */
  Bool forking;                 /* thread currently calling fork? */
  thread_port_t port;           /* thread kernel port */
} ThreadStruct;


Bool ThreadCheck(Thread thread)
{
  CHECKS(Thread, thread);
  CHECKU(Arena, thread->arena);
  CHECKL(thread->serial < thread->arena->threadSerial);
  CHECKD_NOSIG(Ring, &thread->arenaRing);
  CHECKL(BoolCheck(thread->alive));
  CHECKL(BoolCheck(thread->forking));
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

  res = ControlAlloc(&p, arena, sizeof(ThreadStruct));
  if (res != ResOK)
    return res;
  thread = (Thread)p;

  thread->arena = arena;
  RingInit(&thread->arenaRing);

  thread->serial = arena->threadSerial;
  ++arena->threadSerial;
  thread->alive = TRUE;
  thread->forking = FALSE;
  thread->port = mach_thread_self();
  AVER(MACH_PORT_VALID(thread->port));
  thread->sig = ThreadSig;
  AVERT(Thread, thread);

  ProtThreadRegister();

  ring = ArenaThreadRing(arena);

  RingAppend(ring, &thread->arenaRing);

  *threadReturn = thread;
  return ResOK;
}


void ThreadDeregister(Thread thread, Arena arena)
{
  AVERT(Thread, thread);
  AVERT(Arena, arena);
  AVER(!thread->forking);

  RingRemove(&thread->arenaRing);

  thread->sig = SigInvalid;

  RingFinish(&thread->arenaRing);

  ControlFree(arena, thread, sizeof(ThreadStruct));
}


/* mapThreadRing -- map over threads on ring calling a function on
 * each one.
 *
 * Threads that are found to be dead (that is, if func returns FALSE)
 * are marked as dead and moved to deadRing, in order to implement
 * design.thread-manager.sol.thread.term.attempt.
 */

static void mapThreadRing(Ring threadRing, Ring deadRing, Bool (*func)(Thread))
{
  Ring node, next;

  AVERT(Ring, threadRing);
  AVERT(Ring, deadRing);
  AVER(FUNCHECK(func));

  RING_FOR(node, threadRing, next) {
    Thread thread = RING_ELT(Thread, arenaRing, node);
    AVERT(Thread, thread);
    AVER(thread->alive);
    if (!(*func)(thread)) {
      thread->alive = FALSE;
      RingRemove(&thread->arenaRing);
      RingAppend(deadRing, &thread->arenaRing);
    }
  }
}


static Bool threadSuspend(Thread thread)
{
  kern_return_t kern_return;
  mach_port_t self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  if (thread->port == self)
    return TRUE;

  kern_return = thread_suspend(thread->port);
  /* No rendezvous is necessary: thread_suspend "prevents the thread
   * from executing any more user-level instructions" */
  AVER(kern_return == KERN_SUCCESS);
  /* Experimentally, values other then KERN_SUCCESS indicate the thread has
     terminated <https://info.ravenbrook.com/mail/2014/10/25/18-12-36/0/>. */
  /* design.thread-manager.sol.thread.term.attempt */
  return kern_return == KERN_SUCCESS;
}


/* ThreadRingSuspend -- suspend all threads on a ring, except the
 * current one.
 */
void ThreadRingSuspend(Ring threadRing, Ring deadRing)
{
  mapThreadRing(threadRing, deadRing, threadSuspend);
}


static Bool threadResume(Thread thread)
{
  kern_return_t kern_return;
  mach_port_t self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  if (thread->port == self)
    return TRUE;

  kern_return = thread_resume(thread->port);
  /* Mach has no equivalent of EAGAIN. */
  AVER(kern_return == KERN_SUCCESS);
  /* Experimentally, values other then KERN_SUCCESS indicate the thread has
     terminated <https://info.ravenbrook.com/mail/2014/10/25/18-12-36/0/>. */
  /* design.thread-manager.sol.thread.term.attempt */
  return kern_return == KERN_SUCCESS;
}

/* ThreadRingResume -- resume all threads on a ring, except the
 * current one.
 */
void ThreadRingResume(Ring threadRing, Ring deadRing)
{
  mapThreadRing(threadRing, deadRing, threadResume);
}


Thread ThreadRingThread(Ring threadRing)
{
  Thread thread;
  AVERT(Ring, threadRing);
  thread = RING_ELT(Thread, arenaRing, threadRing);
  AVERT(Thread, thread);
  return thread;
}


/* Must be thread-safe. See <design/interface-c/#check.testt>. */

Arena ThreadArena(Thread thread)
{
  AVER(TESTT(Thread, thread));
  return thread->arena;
}


/* ThreadScan -- scan the state of a thread (stack and regs) */

#include "prmcxc.h"

Res ThreadScan(ScanState ss, Thread thread, void *stackCold,
               mps_area_scan_t scan_area, void *closure)
{
  mach_port_t self;
  Res res;

  AVERT(Thread, thread);
  self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  if (thread->port == self) {
    /* scan this thread's stack */
    AVER(thread->alive);
    res = StackScan(ss, stackCold, scan_area, closure);
    if(res != ResOK)
      return res;
  } else if (thread->alive) {
    MutatorContextStruct context;
    THREAD_STATE_S threadState;
    Word *stackBase, *stackLimit;
    Addr stackPtr;
    mach_msg_type_number_t count;
    kern_return_t kern_return;

    /* Note: We could get the thread state and check the suspend count in
       order to assert that the thread is suspended, but it's probably
       unnecessary and is a lot of work to check a static condition. */

    MutatorContextInitThread(&context, &threadState);

    count = THREAD_STATE_COUNT;
    AVER(sizeof(*context.threadState) == count * sizeof(natural_t));
    kern_return = thread_get_state(thread->port,
                                   THREAD_STATE_FLAVOR,
                                   (thread_state_t)context.threadState,
                                   &count);
    AVER(kern_return == KERN_SUCCESS);
    AVER(count == THREAD_STATE_COUNT);

    stackPtr = MutatorContextSP(&context);
    /* .stack.align */
    stackBase  = (Word *)AddrAlignUp(stackPtr, sizeof(Word));
    stackLimit = stackCold;
    if (stackBase >= stackLimit)
      return ResOK;    /* .stack.below-bottom */

    /* scan stack inclusive of current sp and exclusive of
     * stackCold (.stack.full-descend)
     */
    res = TraceScanArea(ss, stackBase, stackLimit,
                        scan_area, closure);
    if(res != ResOK)
      return res;

    /* scan the registers in the mutator context */
    res = MutatorContextScan(ss, &context, scan_area, closure);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}


Res ThreadDescribe(Thread thread, mps_lib_FILE *stream, Count depth)
{
  Res res;

  res = WriteF(stream, depth,
               "Thread $P ($U) {\n", (WriteFP)thread, (WriteFU)thread->serial,
               "  arena $P ($U)\n",
               (WriteFP)thread->arena, (WriteFU)thread->arena->serial,
               "  alive $S\n", WriteFYesNo(thread->alive),
               "  port $U\n", (WriteFU)thread->port,
               "} Thread $P ($U)\n", (WriteFP)thread, (WriteFU)thread->serial,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


/* threadAtForkPrepare -- for each arena, mark the current thread as
 * forking <design/thread-safety/#sol.fork.thread>.
 */

static Bool threadForkPrepare(Thread thread)
{
  mach_port_t self;
  AVERT(Thread, thread);
  AVER(!thread->forking);
  self = mach_thread_self();
  AVER(MACH_PORT_VALID(self));
  thread->forking = (thread->port == self);
  return TRUE;
}

static void threadRingForkPrepare(Arena arena)
{
  AVERT(Arena, arena);
  mapThreadRing(ArenaThreadRing(arena), ArenaDeadRing(arena), threadForkPrepare);
}

static void threadAtForkPrepare(void)
{
  GlobalsArenaMap(threadRingForkPrepare);
}


/* threadAtForkParent -- for each arena, clear the forking flag for
 * all threads <design/thread-safety/#sol.fork.thread>.
 */

static Bool threadForkParent(Thread thread)
{
  AVERT(Thread, thread);
  thread->forking = FALSE;
  return TRUE;
}

static void threadRingForkParent(Arena arena)
{
  AVERT(Arena, arena);
  mapThreadRing(ArenaThreadRing(arena), ArenaDeadRing(arena), threadForkParent);
}

static void threadAtForkParent(void)
{
  GlobalsArenaMap(threadRingForkParent);
}


/* threadAtForkChild -- For each arena, move all threads to the dead
 * ring, except for the thread that was marked as forking by the
 * prepare handler <design/thread-safety/#sol.fork.thread>, for which
 * update its mach port <design/thread-safety/#sol.fork.mach-port>.
 */

static Bool threadForkChild(Thread thread)
{
  AVERT(Thread, thread);
  if (thread->forking) {
    thread->port = mach_thread_self();
    AVER(MACH_PORT_VALID(thread->port));
    thread->forking = FALSE;
    return TRUE;
  } else {
    return FALSE;
  }
}

static void threadRingForkChild(Arena arena)
{
  AVERT(Arena, arena);
  mapThreadRing(ArenaThreadRing(arena), ArenaDeadRing(arena), threadForkChild);
}

static void threadAtForkChild(void)
{
  GlobalsArenaMap(threadRingForkChild);
}

void ThreadSetup(void)
{
  pthread_atfork(threadAtForkPrepare, threadAtForkParent, threadAtForkChild);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

/*  impl.h.th
 *
 *                    THREAD MANAGER
 *
 *  $HopeName: MMsrc!th.h(trunk.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Provides stack scanning and thread suspension facilities.
 *  See design.mps.thread-manager
 * 
 *  This provides facilities for suspending/resuming mutator threads;
 *  scanning of the mutator's stacks.  Each thread has to be individually
 *  registered and deregistered with a space.
 */

#ifndef th_h
#define th_h

#include "std.h"


/*  == Thread Type ==
 *
 *  A Thread is a handle returned by ThreadRegister which must be
 *  used for deregistration.
 */

typedef struct ThreadStruct *Thread;


#include "pool.h"
#include "trace.h"
#include "ref.h"


extern Bool ThreadIsValid(Thread thread, ValidationType validParam);


/*  == Register/Deregister ==
 * 
 *  Explicitly register/deregister a thread on the space threadDeque.
 *  stackBot explicitly records the bottom of stack for this 
 *  thread.  Register returns a "Thread" value which needs to be used
 *  for deregistration.
 *
 *  Threads must not be multiply registered in the same space.
 */

extern Error ThreadRegister(Thread *threadReturn,
                            Space space, Addr *stackBot);

extern void ThreadDeregister(Thread thread, Space space);


/*  == ThreadDequeSuspend/Resume ==
 *
 *  These functions suspend/resume the threads on the deque.
 *  If the current thread is among them, it is not suspended,
 *  nor is any attempt to resume it made.
 */

extern void ThreadDequeSuspend(Deque threadDeque);
extern void ThreadDequeResume(Deque threadDeque);


/*  == ThreadDequeScan ==
 *
 *  This function has the type of a root scanning function.
 *
 *  It must be called with the closure variable p set to
 *  (void *)threadDeque, where threadDeque is
 *  a deque of threads.
 *
 *  This ambiguously scans the stacks of the threads on the
 *  deque between the stack level defined on registration
 *  and the thread's current stack level.  It also fixes
 *  their root registers.
 *
 *  The scan is inclusive/exclusive of stackBot if the stack
 *  is by convention empty/full.
 */

extern Error ThreadDequeScan(void *p, int i, Trace trace);

/*  == ThreadSpace == */

extern Space ThreadSpace(Thread thread);


#endif /* th_h */

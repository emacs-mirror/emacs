/*  impl.h.th
 *
 *                    THREAD MANAGER
 *
 *  $HopeName: MMsrc!th.h(trunk.3) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Provides thread suspension facilities to the shield.
 *  See design.mps.thread-manager.  Each thread has to be
 *  individually registered and deregistered with a space.
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
 *  Register returns a "Thread" value which needs to be used
 *  for deregistration.
 *
 *  Threads must not be multiply registered in the same space.
 */

extern Error ThreadRegister(Thread *threadReturn, Space space);

extern void ThreadDeregister(Thread thread, Space space);


/*  == ThreadDequeSuspend/Resume ==
 *
 *  These functions suspend/resume the threads on the deque.
 *  If the current thread is among them, it is not suspended,
 *  nor is any attempt to resume it made.
 */

extern void ThreadDequeSuspend(Deque threadDeque);
extern void ThreadDequeResume(Deque threadDeque);


extern Space ThreadSpace(Thread thread);

extern Error ThreadScan(ScanState ss, Thread thread, void *stackBot);


#endif /* th_h */

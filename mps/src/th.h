/*  impl.h.th
 *                       THREADS
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Provides stack scanning and thread suspension facilities.
 * 
 *  This provides facilities for suspending/resuming mutator threads;
 *  scanning of the mutator's stacks.  Each thread has to be individually
 *  registered and deregistered.  It also provides a trampoline
 *  for calling into the memory managed world.
 */

#ifndef th_h
#define th_h

#include "std.h"

/*  == Threads ==
 *
 *  Threads is the type of a handle on the mutator's threads.  Threads
 *  are registered with the space which contains a Threads structure.
 *  These registered threads are the ones identified for stack scanning,
 *  resuming and suspending.
 */  

typedef struct ThreadsStruct *Threads;

/*  == Thread Type ==
 *
 *  A Thread is a handle returned by ThreadRegister which must be
 *  used for deregistration and when using the Trampette
 */

typedef struct ThreadStruct *Thread;


#include "pool.h"
#include "trace.h"
#include "ref.h"


extern Bool ThreadsIsValid(Threads threads, ValidationType validParam);

extern Bool ThreadIsValid(Thread thread, ValidationType validParam);

extern Error ThreadsCreate(Threads *threadsReturn, Space space);

extern void ThreadsDestroy(Threads threads, Space space);


/*  == Register/Dergister ==
 * 
 *  Explicitly register/deregister a thread with a space.  The trampette
 *  must be used before accessing automatically managed memory. 
 *  stackBot explicitly records the bottom of stack for this thread.
 *  The thread should not be registered multiple times with a space.
 */

extern Error ThreadRegister(Thread *threadReturn,
                            Addr *stackBot, Space space);

extern void ThreadDeregister(Thread thread, Space space);


/*  == Trampoline ==
 *
 *  ThreadTrampoline calls the function "function" with argument arg.
 *  The result is placed in valueReturn.
 *
 *  thread must be the object returned by registration of the current
 *  thread.
 *
 *  The (inclusive) area between each current stack
 *  pointer and the value of the stack pointer on entry to program
 *  are treated as an array of ambiguous roots.  On certain platforms
 *  registers may also contain roots.
 *
 *  Whether access to managed memory is allowed outside the trampoline
 *  is defined on a per pool basis.  In general, access is disallowed
 *  for automatically managed memory.
 */

typedef void *(*ThreadContinue)(void *arg);

extern void (ThreadTrampoline)(void **valueReturn,
                               Thread thread,
                               ThreadContinue function,
                               void *arg);

/*  == ThreadsSuspend/ThreadsResume ==
 *
 *  These functions suspend/resume registered threads.  If the current
 *  thread is among them, it is not suspended, nor is any attempt to
 *  resume it made.
 */

extern void ThreadsSuspend(Threads threads);
extern void ThreadsResume(Threads threads);

/*  == ScanStacks ==
 *
 *  This ambiguously scans the stacks of the registered threads
 *  between the stack
 *  level defined on registration
 *  and the thread's current stack level.
 */

extern Error ThreadsScanStacks(void *p, int i, Trace trace);

#endif /* th_h */

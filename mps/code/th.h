/*  impl.h.th: THREAD MANAGER
 *
 *  $HopeName$
 *  Copyright (C) 1995 Harlequin Limited.  All rights reserved.
 *
 *  .purpose: Provides thread suspension facilities to the shield.
 *  See design.mps.thread-manager.  Each thread has to be
 *  individually registered and deregistered with an arena.
 */

#ifndef th_h
#define th_h

#include "mpmtypes.h"
#include "ring.h"


#define ThreadSig       ((Sig)0x519286ED) /* SIGnature THREaD */

extern Bool ThreadCheck(Thread thread);


/*  ThreadCheckSimple
 *
 *  Simple thread-safe check of a thread object.
 */

extern Bool ThreadCheckSimple(Thread thread);


extern Res ThreadDescribe(Thread thread, mps_lib_FILE *stream);


/*  Register/Deregister
 *
 *  Explicitly register/deregister a thread on the arena threadRing.
 *  Register returns a "Thread" value which needs to be used
 *  for deregistration.
 *
 *  Threads must not be multiply registered in the same arena.
 */

extern Res ThreadRegister(Thread *threadReturn, Arena arena);

extern void ThreadDeregister(Thread thread, Arena arena);


/*  ThreadRingSuspend/Resume
 *
 *  These functions suspend/resume the threads on the ring.
 *  If the current thread is among them, it is not suspended,
 *  nor is any attempt to resume it made.
 */

extern void ThreadRingSuspend(Ring threadRing);
extern void ThreadRingResume(Ring threadRing);


/*  ThreadRingThread 
 *
 *  Return the thread from an element of the Arena's
 *  thread ring.
 */

extern Thread ThreadRingThread(Ring threadRing);


extern Arena ThreadArena(Thread thread);

extern Res ThreadScan(ScanState ss, Thread thread, void *stackBot);


#endif /* th_h */

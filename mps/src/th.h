/*  impl.h.th
 *
 *                    THREAD MANAGER
 *
 *  $HopeName: MMsrc!th.h(MMdevel_lib.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  Provides thread suspension facilities to the shield.
 *  See design.mps.thread-manager.  Each thread has to be
 *  individually registered and deregistered with a space.
 */

#ifndef th_h
#define th_h

#include "mpm.h"

extern Bool ThreadCheck(Thread thread);
extern Res ThreadDescribe(Thread thread, mps_lib_FILE *stream);


/*  == Register/Deregister ==
 *
 *  Explicitly register/deregister a thread on the space threadRing.
 *  Register returns a "Thread" value which needs to be used
 *  for deregistration.
 *
 *  Threads must not be multiply registered in the same space.
 */

extern Res ThreadRegister(Thread *threadReturn, Space space);

extern void ThreadDeregister(Thread thread, Space space);


/*  == ThreadRingSuspend/Resume ==
 *
 *  These functions suspend/resume the threads on the ring.
 *  If the current thread is among them, it is not suspended,
 *  nor is any attempt to resume it made.
 */

extern void ThreadRingSuspend(Ring threadRing);
extern void ThreadRingResume(Ring threadRing);


extern Space ThreadSpace(Thread thread);

extern Res ThreadScan(ScanState ss, Thread thread, void *stackBot);


#endif /* th_h */

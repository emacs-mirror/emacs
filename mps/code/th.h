/* th.h: THREAD MANAGER
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 *  .purpose: Provides thread suspension facilities to the shield.
 *  See <design/thread-manager/>.  Each thread has to be
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


extern Res ThreadDescribe(Thread thread, mps_lib_FILE *stream, Count depth);


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
 *  These functions suspend/resume the threads on the ring. If the
 *  current thread is among them, it is not suspended, nor is any
 *  attempt to resume it made. Threads that can't be suspended/resumed
 *  because they are dead are moved to deadRing.
 */

extern void ThreadRingSuspend(Ring threadRing, Ring deadRing);
extern void ThreadRingResume(Ring threadRing, Ring deadRing);


/*  ThreadRingThread
 *
 *  Return the thread from an element of the Arena's
 *  thread ring.
 */

extern Thread ThreadRingThread(Ring threadRing);


extern Arena ThreadArena(Thread thread);

extern Res ThreadScan(ScanState ss, Thread thread, void *stackCold,
                      mps_area_scan_t scan_area,
                      void *closure);


#endif /* th_h */


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

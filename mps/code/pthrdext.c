/* pthreadext.c: POSIX THREAD EXTENSIONS
 *
 *  $Id$
 *  Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Provides extension to Pthreads.
 *
 * .design: see <design/pthreadext/>
 *
 * .acknowledgements: This was derived from code posted to
 * comp.programming.threads by Dave Butenhof and Raymond Lau
 * (<David.Butenhof@compaq.com>, <rlau@csc.com>).
 */


#include "mpm.h"

#include <pthread.h>
#include <sched.h>
#include <signal.h> /* see .feature.li in config.h */
#include <semaphore.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "pthrdext.h"

SRCID(pthreadext, "$Id$");


/* Static data initialized on first use of the module
 * See <design/pthreadext/#impl.static>.*
 */

/* mutex */
static pthread_mutex_t pthreadextMut = PTHREAD_MUTEX_INITIALIZER;

/* semaphore */
static sem_t pthreadextSem;

/* initialization support */
static pthread_once_t pthreadextOnce = PTHREAD_ONCE_INIT;
static Bool pthreadextModuleInitialized = FALSE;


/* Global variables protected by the mutex
 * See <design/pthreadext/#impl.global>.*
 */

static PThreadext suspendingVictim = NULL;  /* current victim */
static RingStruct suspendedRing;            /* PThreadext suspend ring */


/* suspendSignalHandler -- signal handler called when suspending a thread
 *
 * See <design/pthreadext/#impl.suspend-handler>
 *
 * The interface for determining the MFC might be platform specific.
 *
 * Handle PTHREADEXT_SIGSUSPEND in the target thread, to suspend it until
 * receiving PTHREADEXT_SIGRESUME (resume). Note that this is run with both
 * PTHREADEXT_SIGSUSPEND and PTHREADEXT_SIGRESUME blocked. Having
 * PTHREADEXT_SIGRESUME blocked prevents a resume before we can finish the
 * suspend protocol.
 */

#include "prmcix.h"

static void suspendSignalHandler(int sig,
                                 siginfo_t *info,
                                 void *uap)
{
    sigset_t signal_set;
    ucontext_t ucontext;
    MutatorContextStruct context;

    AVER(sig == PTHREADEXT_SIGSUSPEND);
    UNUSED(sig);
    UNUSED(info);

    AVER(suspendingVictim != NULL);
    /* copy the ucontext structure so we definitely have it on our stack,
     * not (e.g.) shared with other threads. */
    ucontext = *(ucontext_t *)uap;
    context.ucontext = &ucontext;
    suspendingVictim->context = &context;
    /* Block all signals except PTHREADEXT_SIGRESUME while suspended. */
    sigfillset(&signal_set);
    sigdelset(&signal_set, PTHREADEXT_SIGRESUME);
    sem_post(&pthreadextSem);
    sigsuspend(&signal_set);

    /* Once here, the resume signal handler has run to completion. */
    return;
}


/* resumeSignalHandler -- signal handler called when resuming a thread
 *
 * See <design/pthreadext/#impl.suspend-handler>
 */

static void resumeSignalHandler(int sig)
{
    AVER(sig == PTHREADEXT_SIGRESUME);
    UNUSED(sig);
    return;
}

/* PThreadextModuleInit -- Initialize the PThreadext module
 *
 * See <design/pthreadext/#impl.static.init>
 *
 * Dynamically initialize all state when first used
 * (called by pthread_once).
 */

static void PThreadextModuleInit(void)
{
    int status;
    struct sigaction pthreadext_sigsuspend, pthreadext_sigresume;
  
    AVER(pthreadextModuleInitialized == FALSE);

    /* Initialize the ring of suspended threads */
    RingInit(&suspendedRing);

    /* Initialize the semaphore */
    status = sem_init(&pthreadextSem, 0, 0);
    AVER(status != -1);

    /* Install the signal handlers for suspend/resume. */
    /* We add PTHREADEXT_SIGRESUME to the sa_mask field for the */
    /* PTHREADEXT_SIGSUSPEND handler. That avoids a race if one thread */
    /* suspends the target while another resumes that same target. (The */
    /* PTHREADEXT_SIGRESUME signal cannot be delivered before the */
    /* target thread calls sigsuspend.) */

    status = sigemptyset(&pthreadext_sigsuspend.sa_mask);
    AVER(status == 0);
    status = sigaddset(&pthreadext_sigsuspend.sa_mask, PTHREADEXT_SIGRESUME);
    AVER(status == 0);

    pthreadext_sigsuspend.sa_flags = SA_SIGINFO;
    pthreadext_sigsuspend.sa_sigaction = suspendSignalHandler;
    pthreadext_sigresume.sa_flags = 0;
    pthreadext_sigresume.sa_handler = resumeSignalHandler;
    status = sigemptyset(&pthreadext_sigresume.sa_mask);
    AVER(status == 0);

    status = sigaction(PTHREADEXT_SIGSUSPEND, &pthreadext_sigsuspend, NULL);
    AVER(status == 0);

    status = sigaction(PTHREADEXT_SIGRESUME, &pthreadext_sigresume, NULL);
    AVER(status == 0);

    pthreadextModuleInitialized = TRUE;
}


/* PThreadextCheck -- check the consistency of a PThreadext structure */

extern Bool PThreadextCheck(PThreadext pthreadext)
{
  int status;

  status = pthread_mutex_lock(&pthreadextMut);
  AVER(status == 0);

  CHECKS(PThreadext, pthreadext);
  /* can't check ID */
  CHECKD_NOSIG(Ring, &pthreadext->threadRing);
  CHECKD_NOSIG(Ring, &pthreadext->idRing);
  if (pthreadext->context == NULL) {
    /* not suspended */
    CHECKL(RingIsSingle(&pthreadext->threadRing));
    CHECKL(RingIsSingle(&pthreadext->idRing));
  } else {
    /* suspended */
    Ring node, next;
    CHECKL(!RingIsSingle(&pthreadext->threadRing));
    RING_FOR(node, &pthreadext->idRing, next) {
      PThreadext pt = RING_ELT(PThreadext, idRing, node);
      CHECKL(pt->id == pthreadext->id);
      CHECKL(pt->context == pthreadext->context);
    }
  }
  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);

  return TRUE;
}


/*  PThreadextInit -- Initialize a pthreadext */

extern void PThreadextInit(PThreadext pthreadext, pthread_t id)
{
  int status;

  /* The first call to init will initialize the package. */
  status = pthread_once(&pthreadextOnce, PThreadextModuleInit);
  AVER(status == 0);

  pthreadext->id = id;
  pthreadext->context = NULL;
  RingInit(&pthreadext->threadRing);
  RingInit(&pthreadext->idRing);
  pthreadext->sig = PThreadextSig;
  AVERT(PThreadext, pthreadext);
}


/* PThreadextFinish -- Finish a pthreadext
 *
 * See <design/pthreadext/#impl.finish>
 */

extern void PThreadextFinish(PThreadext pthreadext)
{
  int status;

  AVERT(PThreadext, pthreadext);

  status = pthread_mutex_lock(&pthreadextMut);
  AVER(status == 0);

  if(pthreadext->context == NULL) {
    AVER(RingIsSingle(&pthreadext->threadRing));
    AVER(RingIsSingle(&pthreadext->idRing));
  } else {
    /* In suspended state: remove from rings. */
    AVER(!RingIsSingle(&pthreadext->threadRing));
    RingRemove(&pthreadext->threadRing);
    if(!RingIsSingle(&pthreadext->idRing))
      RingRemove(&pthreadext->idRing);
  }

  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);

  RingFinish(&pthreadext->threadRing);
  RingFinish(&pthreadext->idRing);
  pthreadext->sig = SigInvalid;
}


/* PThreadextSuspend -- suspend a thread
 *
 * See <design/pthreadext/#impl.suspend>
 */

Res PThreadextSuspend(PThreadext target, MutatorContext *contextReturn)
{
  Ring node, next;
  Res res;
  int status;

  AVERT(PThreadext, target);
  AVER(contextReturn != NULL);
  AVER(target->context == NULL); /* multiple suspends illegal */

  /* Serialize access to suspend, makes life easier */
  status = pthread_mutex_lock(&pthreadextMut);
  AVER(status == 0);
  AVER(suspendingVictim == NULL);

  /* Threads are added to the suspended ring on suspension */
  /* If the same thread Id has already been suspended, then */
  /* don't signal the thread, just add the target onto the id ring */
  RING_FOR(node, &suspendedRing, next) {
    PThreadext alreadySusp = RING_ELT(PThreadext, threadRing, node);
    if (alreadySusp->id == target->id) {
      RingAppend(&alreadySusp->idRing, &target->idRing);
      target->context = alreadySusp->context;
      goto noteSuspended;
    }
  }

  /* Ok, we really need to suspend this thread. */
  suspendingVictim = target;
  status = pthread_kill(target->id, PTHREADEXT_SIGSUSPEND);
  if (status != 0) {
    res = ResFAIL;
    goto unlock;
  }

  /* Wait for the victim to acknowledge suspension. */
  while (sem_wait(&pthreadextSem) != 0) {
    if (errno != EINTR) {
      res = ResFAIL;
      goto unlock;
    }
  }

noteSuspended:
  AVER(target->context != NULL);
  RingAppend(&suspendedRing, &target->threadRing);
  *contextReturn = target->context;
  res = ResOK;

unlock:
  suspendingVictim = NULL;
  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);
  return res;
}


/* PThreadextResume -- resume a suspended thread
 *
 * See <design/pthreadext/#impl.resume>
 */

Res PThreadextResume(PThreadext target)
{
  Res res;
  int status;

  AVERT(PThreadext, target);
  AVER(pthreadextModuleInitialized);  /* must have been a prior suspend */
  AVER(target->context != NULL);

  /* Serialize access to suspend, makes life easier. */
  status = pthread_mutex_lock(&pthreadextMut);
  AVER(status == 0);

  if (RingIsSingle(&target->idRing)) {
    /* Really want to resume the thread. Signal it to continue. */
    status = pthread_kill(target->id, PTHREADEXT_SIGRESUME);
    if (status == 0) {
      goto noteResumed;
    } else {
      res = ResFAIL;
      goto unlock;
    }

  } else {
    /* Leave thread suspended on behalf of another PThreadext. */
    /* Remove it from the id ring */
    RingRemove(&target->idRing);
    goto noteResumed;
  }

noteResumed:
  /* Remove the thread from the suspended ring */
  RingRemove(&target->threadRing);
  target->context = NULL;
  res = ResOK;

unlock:
  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

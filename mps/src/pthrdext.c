/*  impl.c.pthreadext: POSIX THREAD EXTENSIONS
 *
 *  $HopeName: MMsrc!pthrdext.c(trunk.2) $
 *  Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .purpose: Provides extension to Pthreads.
 *
 * .design: see design.mps.pthreadext
 *
 * .acknowledgements: This was derived from code posted to
 * comp.programming.threads by Dave Butenhof and Raymond Lau
 * (<David.Butenhof@compaq.com>, <rlau@csc.com>).
 */


/* open sesame magic */
#define _BSD_SOURCE 1
#define _POSIX_C_SOURCE 1

#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <semaphore.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "pthrdext.h"
#include "mpm.h"

SRCID(pthreadext, "$HopeName$");


/* PTHREADEXT_SIGSUSPEND, PTHREADEXT_SIGRESUME -- signals used
 *
 * See design.mps.pthreadext.impl.signals
 */

#define PTHREADEXT_SIGSUSPEND SIGXFSZ
#define PTHREADEXT_SIGRESUME SIGPWR


/* Static data initiatialized on first use of the module
 * See design.mps.pthreadext.impl.static.*
 */

/* mutex */
static pthread_mutex_t pthreadextMut = PTHREAD_MUTEX_INITIALIZER;

/* semaphore */
static sem_t pthreadextSem;

/* initialization support */
static pthread_once_t pthreadextOnce = PTHREAD_ONCE_INIT;
static Bool pthreadextModuleInitialized = FALSE;


/* Global variables protected by the mutex
 * See design.mps.pthreadext.impl.global.*
 */

static PThreadext suspendingVictim = NULL;  /* current victim */
static RingStruct suspendedRing;            /* PThreadext suspend ring */


static void suspendSignalHandler(int sig, struct sigcontext scp);
static void resumeSignalHandler(int sig);


/* PThreadextModuleInit -- Initialize the PThreadext module
 *
 * See design.mps.pthreadext.impl.static.init
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

    pthreadext_sigsuspend.sa_flags = 0;
    pthreadext_sigsuspend.sa_handler = (__sighandler_t)suspendSignalHandler;
    status = sigemptyset(&pthreadext_sigsuspend.sa_mask);
    AVER(status == 0);
    status = sigaddset(&pthreadext_sigsuspend.sa_mask, PTHREADEXT_SIGRESUME);
    AVER(status == 0);

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
  CHECKL(RingCheck(&pthreadext->threadRing));
  CHECKL(RingCheck(&pthreadext->idRing));
  if (pthreadext->suspendedScp == NULL) {
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
      CHECKL(pt->suspendedScp == pthreadext->suspendedScp);
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
  pthreadext->suspendedScp = NULL;
  RingInit(&pthreadext->threadRing);
  RingInit(&pthreadext->idRing);
  pthreadext->sig = PThreadextSig;
  AVERT(PThreadext, pthreadext);
}


/* PThreadextFinish -- Finish a pthreadext
 *
 * See design.mps.pthreadext.impl.finish
 */

extern void PThreadextFinish(PThreadext pthreadext)
{
  int status;

  AVERT(PThreadext, pthreadext);

  status = pthread_mutex_lock(&pthreadextMut);
  AVER(status == 0);

  if(pthreadext->suspendedScp == NULL) {
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


/* suspendSignalHandler -- signal handler called when suspending a thread
 *
 * See design.mps.pthreadext.impl.suspend-handler
 *
 * The interface for determining the sigcontext might be platform specific.
 *
 * Handle PTHREADEXT_SIGSUSPEND in the target thread, to suspend it until
 * receiving PTHREADEXT_SIGRESUME (resume). Note that this is run with both
 * PTHREADEXT_SIGSUSPEND and PTHREADEXT_SIGRESUME blocked. Having
 * PTHREADEXT_SIGRESUME blocked prevents a resume before we can finish the
 * suspend protocol.
 */

static void suspendSignalHandler(int sig, struct sigcontext scp)
{
    sigset_t signal_set;

    AVER(sig == PTHREADEXT_SIGSUSPEND);
    UNUSED(sig);

    /* Tell caller about the sigcontext. */
    AVER(suspendingVictim != NULL);
    suspendingVictim->suspendedScp = &scp;

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
 * See design.mps.pthreadext.impl.suspend-handler
 */

static void resumeSignalHandler(int sig)
{
    AVER(sig == PTHREADEXT_SIGRESUME);
    UNUSED(sig);
    return;
}


/* PThreadextSuspend -- suspend a thread
 *
 * See design.mps.pthreadext.impl.suspend
 */

Res PThreadextSuspend(PThreadext target, struct sigcontext **contextReturn)
{
  Ring node, next;
  Res res;
  int status;

  AVERT(PThreadext, target);
  AVER(contextReturn != NULL);
  AVER(target->suspendedScp == NULL); /* multiple suspends illegal */

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
      target->suspendedScp = alreadySusp->suspendedScp;
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
  AVER(target->suspendedScp != NULL);
  RingAppend(&suspendedRing, &target->threadRing);
  *contextReturn = target->suspendedScp;
  res = ResOK;

unlock:
  suspendingVictim = NULL;
  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);
  return res;
}


/* PThreadextResume -- resume a suspended thread
 * 
 * See design.mps.pthreadext.impl.resume
 */

Res PThreadextResume(PThreadext target)
{
  Res res;
  int status;

  AVERT(PThreadext, target);
  AVER(pthreadextModuleInitialized);  /* must have been a prior suspend */
  AVER(target->suspendedScp != NULL);

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
  target->suspendedScp = NULL;
  res = ResOK;

unlock:
  status = pthread_mutex_unlock(&pthreadextMut);
  AVER(status == 0);
  return res;
}

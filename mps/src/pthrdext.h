/*  impl.h.pthreadext: POSIX THREAD EXTENSIONS
 *
 *  $HopeName: !pthrdext.h(trunk.1) $
 *  Copyright (C) 2000 Harlequin Ltd, all rights reserved
 *
 * .readership: MM developers.
 *
 *  .purpose: Provides extension to Pthreads.
 */

#ifndef pthreadext_h
#define pthreadext_h

#include <signal.h>

#include "mpm.h"


#define PThreadextSig ((Sig)0x519B286E) /* SIGnature PTHReadExt */


/* PThreadext -- extension datatype  */

typedef struct PThreadextStruct *PThreadext;


/* PThreadextStruct -- structure definition
 *
 * Should be embedded in a client structure
 */

typedef struct PThreadextStruct {
  Sig sig;                         /* design.mps.sig */
  pthread_t id;                    /* Thread ID */
  MutatorFaultContext suspendedMFC; /* context if suspended */
  RingStruct threadRing;           /* ring of suspended threads */
  RingStruct idRing;               /* duplicate suspensions for id */
} PThreadextStruct;



/*  PThreadextCheck -- Check a pthreadext */

extern Bool PThreadextCheck(PThreadext pthreadext);


/*  PThreadextInit -- Initialize a pthreadext */

extern void PThreadextInit(PThreadext pthreadext, pthread_t id);


/*  PThreadextFinish -- Finish a pthreadext */

extern void PThreadextFinish(PThreadext pthreadext);


/*  PThreadextSuspend -- Suspend a pthreadext and return its context. */

extern Res PThreadextSuspend(PThreadext pthreadext,
 			     MutatorFaultContext *contextReturn);

/*  PThreadextResume --  Resume a suspended pthreadext */

extern Res PThreadextResume(PThreadext pthreadext);


#endif /* pthreadext_h */

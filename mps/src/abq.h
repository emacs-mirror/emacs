/* impl.h.abq: ABQ INTERFACE
 *
 * $HopeName: MMsrc!abq.h(trunk.3) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: A FIFO queue substrate for impl.c.poolmv2
 *
 * .source: design.mps.poolmv2
 */

#ifndef abq_h
#define abq_h

#include "meter.h"
#include "cbs.h"
#include "mpm.h"


/* Signatures */

#define ABQSig ((Sig)0x519AB099) /* SIGnature ABQ */


/* Prototypes  */

typedef struct ABQStruct *ABQ;
extern Res ABQInit(Arena arena, ABQ abq, void *owner, Count items);
extern Bool ABQCheck(ABQ abq);
extern void ABQFinish(Arena arena, ABQ abq);
extern Res ABQPush(ABQ abq, CBSBlock block);
extern Res ABQPop(ABQ abq, CBSBlock *blockReturn);
extern Res ABQPeek(ABQ abq, CBSBlock *blockReturn);
extern Res ABQDelete(ABQ abq, CBSBlock block);
extern Res ABQDescribe(ABQ abq, mps_lib_FILE *stream);
extern Bool ABQIsEmpty(ABQ abq);
extern Bool ABQIsFull(ABQ abq);
extern Count ABQDepth(ABQ abq);


/* Types */

typedef struct ABQStruct
{
  Count elements;
  Index in;
  Index out;
  CBSBlock *queue;

  /* Meter queue depth at each operation */
  METER_DECL(push);
  METER_DECL(pop);
  METER_DECL(peek);
  METER_DECL(delete);
  
  Sig sig;
} ABQStruct;

#endif /* abq_h */


/* impl.h.abq: ABQ INTERFACE
 *
 * $HopeName: MMsrc!abq.h(MMdevel_gavinm_splay.5) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: Any MPS developer
 *
 * .purpose: A FIFO queue substrate for impl.c.poolmvv2
 *
 * .design: See design.mps.poolmv2
 */

#ifndef abq_h
#define abq_h

#include "mpm.h"
#include "meter.h"


/* Signatures */

#define ABQSig ((Sig)0x519AB099) /* SIGnature ABQ */


/* Prototypes  */

typedef struct ABQStruct *ABQ;
extern Res ABQInit(Arena arena, ABQ abq, Count items);
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
}ABQStruct;

#endif /* abq_h */


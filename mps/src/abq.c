/* impl.c.abq: AVAILABLE BLOCK QUEUE
 *
 * $HopeName: MMsrc!abq.c(trunk.4) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .readership: Any MPS developer
 *
 * .purpose: A FIFO queue substrate for impl.c.poolmv2
 *
 * .design: See design.mps.poolmv2
 */

#include "meter.h"
#include "abq.h"
#include "cbs.h"
#include "mpm.h"

SRCID(abq, "$HopeName: MMsrc!abq.c(trunk.4) $");


/* Private prototypes */

static Size ABQQueueSize(Count elements);
static Index ABQNextIndex(ABQ abq, Index index);


/* Methods */

/* ABQInit -- Initialize an ABQ
 *
 * items is the number of items the queue can hold
 */
Res ABQInit(Arena arena, ABQ abq, void *owner, Count items)
{
  Count elements;
  void *p;
  Res res;

  AVERT(Arena, arena);
  AVER(abq != NULL);
  AVER(items > 0);

  elements = items + 1;
  
  res = ControlAlloc(&p, arena, ABQQueueSize(elements), 
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    return res;

  abq->elements = elements;
  abq->in = 0;
  abq->out = 0;
  abq->queue = (CBSBlock *)p;

  METER_INIT(abq->push, "push", owner);
  METER_INIT(abq->pop, "pop", owner);
  METER_INIT(abq->peek, "peek", owner);
  METER_INIT(abq->delete, "delete", owner);
  
  abq->sig = ABQSig;

  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQCheck -- validate an ABQ */
Bool ABQCheck(ABQ abq)
{
  Index index;
  
  CHECKS(ABQ, abq);
  CHECKL(abq->elements > 0);
  CHECKL(abq->in < abq->elements);
  CHECKL(abq->out < abq->elements);
  CHECKL(abq->queue != NULL);
  /* Is this really a local check? */
  for (index = abq->out; index != abq->in; ) {
    CHECKL(CBSBlockCheck(abq->queue[index]));
    if (++index == abq->elements)
      index = 0;
  }

  return TRUE;
}


/* ABQFinish -- finish an ABQ */
void ABQFinish(Arena arena, ABQ abq)
{
  AVERT(Arena, arena);
  AVERT(ABQ, abq);

  METER_EMIT(&abq->push);
  METER_EMIT(&abq->pop);
  METER_EMIT(&abq->peek);
  METER_EMIT(&abq->delete);
  ControlFree(arena, abq->queue, ABQQueueSize(abq->elements));
  
  abq->elements = 0;
  abq->queue = NULL;

  abq->sig = SigInvalid;
}


/* ABQPush -- push a block onto the tail of the ABQ */
Res ABQPush(ABQ abq, CBSBlock block)
{
  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  METER_ACC(abq->push, ABQDepth(abq));

  if (ABQIsFull(abq))
    return ResFAIL;
  
  abq->queue[abq->in] = block;
  abq->in = ABQNextIndex(abq, abq->in);

  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQPop -- pop a block from the head of the ABQ */
Res ABQPop(ABQ abq, CBSBlock *blockReturn)
{
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  METER_ACC(abq->pop, ABQDepth(abq));
  
  if (ABQIsEmpty(abq))
    return ResFAIL;

  *blockReturn = abq->queue[abq->out];
  AVERT(CBSBlock, *blockReturn);

  abq->out = ABQNextIndex(abq, abq->out);
  
  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQPeek -- peek at the head of the ABQ */
Res ABQPeek(ABQ abq, CBSBlock *blockReturn)
{
  AVER(blockReturn != NULL);
  AVERT(ABQ, abq);

  METER_ACC(abq->peek, ABQDepth(abq));

  if (ABQIsEmpty(abq))
    return ResFAIL;

  *blockReturn = abq->queue[abq->out];
  AVERT(CBSBlock, *blockReturn);

  /* Identical to pop, but don't increment out */

  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQDelete -- delete a block from the ABQ */
Res ABQDelete(ABQ abq, CBSBlock block)
{
  Index index, next, elements, in;
  CBSBlock *queue;

  AVERT(ABQ, abq);
  AVERT(CBSBlock, block);

  METER_ACC(abq->delete, ABQDepth(abq));

  index = abq->out;
  in = abq->in;
  elements = abq->elements;
  queue = abq->queue;
  
  while (index != in) {
    if (queue[index] == block) {
      goto found;
    }
    index = ABQNextIndex(abq, index);
  }

  return ResFAIL;

found:
  /* index points to the node to be removed */
  next = ABQNextIndex(abq, index);
  while (next != in) {
    queue[index] = queue[next];
    index = next;
    next = ABQNextIndex(abq, index);
  }
  abq->in = index;
  AVERT(ABQ, abq);
  return ResOK;
}


/* ABQDescribe -- Describe an ABQ */
Res ABQDescribe(ABQ abq, mps_lib_FILE *stream)
{
  Res res;
  Index index;

  AVERT(ABQ, abq);

  AVER(stream != NULL);

  res = WriteF(stream,
	       "ABQ $P\n{\n", (WriteFP)abq,
	       "  elements: $U \n", (WriteFU)abq->elements,
	       "  in: $U \n", (WriteFU)abq->in,
	       "  out: $U \n", (WriteFU)abq->out,
               "  queue: \n",
	       NULL);
  if(res != ResOK)
    return res;

  for (index = abq->out; index != abq->in; ) {
    res = CBSBlockDescribe(abq->queue[index], stream);
    if(res != ResOK)
      return res;
    if (++index == abq->elements)
      index = 0;
  }

  res = WriteF(stream, "\n", NULL);
  if(res != ResOK)
    return res;

  res = METER_WRITE(abq->push, stream);
  if(res != ResOK)
    return res;
  res = METER_WRITE(abq->pop, stream);
  if(res != ResOK)
    return res;
  res = METER_WRITE(abq->peek, stream);
  if(res != ResOK)
    return res;
  res = METER_WRITE(abq->delete, stream);
  if(res != ResOK)
    return res;
  
  res = WriteF(stream, "}\n", NULL);
  if(res != ResOK)
    return res;
  
  return ResOK;
}


/* ABQIsEmpty -- Is an ABQ empty? */
Bool ABQIsEmpty(ABQ abq) 
{
  AVERT(ABQ, abq);

  return abq->out == abq->in;
}


/* ABQIsFull -- Is an ABQ full? */
Bool ABQIsFull(ABQ abq)
{
  AVERT(ABQ, abq);

  return ABQNextIndex(abq, abq->in) == abq->out;
}


/* ABQDepth -- return the number of items in an ABQ */
Count ABQDepth(ABQ abq)
{
  Index out, in;
  
  AVERT(ABQ, abq);
  out = abq->out;
  in = abq->in;

  if (in >= out)
    return in - out;
  else
    return in + abq->elements - out;
}


/* ABQQueueSize -- calculate the storage required for the vector to
   store elements items */
static Size ABQQueueSize(Count elements)
{
  /* strange but true: the sizeof expression calculates the size of a
     single queue element */
  return (Size)(sizeof(((ABQ)NULL)->queue[0]) * elements);
}


/* ABQNextIndex -- calculate the next index into the queue vector from
   the current one */
static Index ABQNextIndex(ABQ abq, Index index)
{
  Index next = index + 1;
  if (next == abq->elements)
    next = 0;
  return next;
}

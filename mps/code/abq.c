/* impl.c.abq: AVAILABLE BLOCK QUEUE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
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

SRCID(abq, "$Id$");


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

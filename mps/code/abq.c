/* abq.c: QUEUE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: A fixed-length FIFO queue.
 *
 * .design: <design/abq/>
 */

#include "meter.h"
#include "abq.h"
#include "mpm.h"

SRCID(abq, "$Id$");


/* Private prototypes */

static Size ABQQueueSize(Count elements, Size elementSize);
static Index ABQNextIndex(ABQ abq, Index index);
static void *ABQElement(ABQ abq, Index index);


/* Methods */

/* ABQInit -- Initialize an ABQ
 *
 * elements is the number of elements the queue can hold
 */
Res ABQInit(Arena arena, ABQ abq, void *owner, Count elements, Size elementSize)
{
  void *p;
  Res res;

  AVERT(Arena, arena);
  AVER(abq != NULL);
  AVER(elements > 0);

  /* Allocate a dummy extra element in order to be able to distinguish
     "empty" from "full" */
  elements = elements + 1;

  res = ControlAlloc(&p, arena, ABQQueueSize(elements, elementSize),
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    return res;

  abq->elements = elements;
  abq->elementSize = elementSize;
  abq->in = 0;
  abq->out = 0;
  abq->queue = p;

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
  CHECKS(ABQ, abq);
  CHECKL(abq->elements > 0);
  CHECKL(abq->elementSize > 0);
  CHECKL(abq->in < abq->elements);
  CHECKL(abq->out < abq->elements);
  CHECKL(abq->queue != NULL);

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
  ControlFree(arena, abq->queue, ABQQueueSize(abq->elements, abq->elementSize));
 
  abq->elements = 0;
  abq->queue = NULL;

  abq->sig = SigInvalid;
}


/* ABQPush -- push an element onto the tail of the ABQ */
Bool ABQPush(ABQ abq, void *element)
{
  AVERT(ABQ, abq);

  METER_ACC(abq->push, ABQDepth(abq));

  if (ABQIsFull(abq))
    return FALSE;
 
  (void)mps_lib_memcpy(ABQElement(abq, abq->in), element, abq->elementSize);
  abq->in = ABQNextIndex(abq, abq->in);

  AVERT(ABQ, abq);
  return TRUE;
}


/* ABQPop -- pop an element from the head of the ABQ */
Bool ABQPop(ABQ abq, void *elementReturn)
{
  AVER(elementReturn != NULL);
  AVERT(ABQ, abq);

  METER_ACC(abq->pop, ABQDepth(abq));
 
  if (ABQIsEmpty(abq))
    return FALSE;

  (void)mps_lib_memcpy(elementReturn, ABQElement(abq, abq->out), abq->elementSize);

  abq->out = ABQNextIndex(abq, abq->out);
 
  AVERT(ABQ, abq);
  return TRUE;
}


/* ABQPeek -- peek at the head of the ABQ */
Bool ABQPeek(ABQ abq, void *elementReturn)
{
  AVER(elementReturn != NULL);
  AVERT(ABQ, abq);

  METER_ACC(abq->peek, ABQDepth(abq));

  if (ABQIsEmpty(abq))
    return FALSE;

  (void)mps_lib_memcpy(elementReturn, ABQElement(abq, abq->out), abq->elementSize);

  /* Identical to pop, but don't increment out */

  AVERT(ABQ, abq);
  return TRUE;
}


/* ABQDescribe -- Describe an ABQ */
Res ABQDescribe(ABQ abq, ABQDescribeElement describeElement, mps_lib_FILE *stream)
{
  Res res;
  Index index;

  if (!TESTT(ABQ, abq)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

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
    res = (*describeElement)(ABQElement(abq, index), stream);
    if(res != ResOK)
      return res;
    index = ABQNextIndex(abq, index);
  }

  res = WriteF(stream, "\n", NULL);
  if(res != ResOK)
    return res;

  METER_WRITE(abq->push, stream);
  METER_WRITE(abq->pop, stream);
  METER_WRITE(abq->peek, stream);
  METER_WRITE(abq->delete, stream);
 
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


/* ABQDepth -- return the number of elements in an ABQ */
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


/* ABQIterate -- call 'iterate' for each element in an ABQ */
void ABQIterate(ABQ abq, ABQIterateMethod iterate, void *closureP, Size closureS)
{
  Index copy, index, in;

  AVERT(ABQ, abq);
  AVER(FUNCHECK(iterate));

  copy = abq->out;
  index = abq->out;
  in = abq->in;
 
  while (index != in) {
    void *element = ABQElement(abq, index);
    Bool delete = FALSE;
    Bool cont;
    cont = (*iterate)(&delete, element, closureP, closureS);
    AVERT(Bool, cont);
    AVERT(Bool, delete);
    if (!delete) {
      if (copy != index)
        (void)mps_lib_memcpy(ABQElement(abq, copy), element, abq->elementSize);
      copy = ABQNextIndex(abq, copy);
    }
    index = ABQNextIndex(abq, index);
    if (!cont)
      break;
  }

  /* If any elements were deleted, need to copy remainder of queue. */
  if (copy != index) {
    while (index != in) {
      (void)mps_lib_memcpy(ABQElement(abq, copy), ABQElement(abq, index),
                           abq->elementSize);
      copy = ABQNextIndex(abq, copy);
      index = ABQNextIndex(abq, index);
    }
    abq->in = copy;
  }

  AVERT(ABQ, abq);
}


/* ABQQueueSize -- calculate the storage required for the vector to
   store the elements */
static Size ABQQueueSize(Count elements, Size elementSize)
{
  return (Size)(elements * elementSize);
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

/* ABQElement -- return pointer to the index'th element in the queue
   vector. */
static void *ABQElement(ABQ abq, Index index) {
  return PointerAdd(abq->queue, index * abq->elementSize);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

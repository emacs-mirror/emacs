/* abq.h: QUEUE INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: A fixed-length FIFO queue.
 *
 * .design: <design/abq>
 */

#ifndef abq_h
#define abq_h

#include "meter.h"
#include "mpm.h"


/* Signatures */

#define ABQSig ((Sig)0x519AB099) /* SIGnature ABQ */


/* Prototypes  */

typedef struct ABQStruct *ABQ;
typedef Res (*ABQDescribeElement)(void *element, mps_lib_FILE *stream, Count depth);
typedef Bool (*ABQVisitor)(Bool *deleteReturn, void *element, void *closure);

extern Res ABQInit(Arena arena, ABQ abq, void *owner, Count elements, Size elementSize);
extern Bool ABQCheck(ABQ abq);
extern void ABQFinish(Arena arena, ABQ abq);
extern Bool ABQPush(ABQ abq, void *element);
extern Bool ABQPop(ABQ abq, void *elementReturn);
extern Bool ABQPeek(ABQ abq, void *elementReturn);
extern Res ABQDescribe(ABQ abq, ABQDescribeElement describeElement, mps_lib_FILE *stream, Count depth);
extern Bool ABQIsEmpty(ABQ abq);
extern Bool ABQIsFull(ABQ abq);
extern Count ABQDepth(ABQ abq);
extern void ABQIterate(ABQ abq, ABQVisitor visitor, void *closure);


/* Types */

typedef struct ABQStruct
{
  Count elements;
  Size elementSize;
  Index in;
  Index out;
  void *queue;

  /* Meter queue depth at each operation */
  METER_DECL(push)
  METER_DECL(pop)
  METER_DECL(peek)
  METER_DECL(delete)

  Sig sig;
} ABQStruct;

#endif /* abq_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

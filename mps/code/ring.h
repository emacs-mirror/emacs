/* ring.h: RING INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 */


#ifndef ring_h
#define ring_h

#include "check.h"
#include "mpmtypes.h"


/* RingStruct -- double-ended queue structure
 *
 * .ring: The ring structure is used as a field in other structures
 * in order to link them together into "rings".  See <code/ring.c>.
 */

typedef struct RingStruct *Ring;
typedef struct RingStruct {     /* double-ended queue structure */
  Ring next, prev;              /* links to next and prev element */
} RingStruct;


#define RingNONE ((Ring)0)

extern Bool RingCheck(Ring ring);
extern Bool RingCheckSingle(Ring ring);
extern Bool RingIsSingle(Ring ring);

/* .ring.init: */
extern void (RingInit)(Ring ring);
#define RingInit(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(_ring != NULL); \
    _ring->next = _ring; \
    _ring->prev = _ring; \
    AVER(RingCheck(_ring)); \
  END

/* .ring.finish: */
extern void (RingFinish)(Ring ring);
#define RingFinish(ring) \
  BEGIN \
    Ring _ring = (ring); \
    AVER(RingCheckSingle(_ring)); \
    _ring->next = RingNONE; \
    _ring->prev = RingNONE; \
  END

/* .ring.append: */
extern void (RingAppend)(Ring ring, Ring new);
#define RingAppend(ring, new) \
  BEGIN \
    Ring _ring = (ring), _new = (new); \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    _new->prev = _ring->prev; \
    _new->next = _ring; \
    _ring->prev->next = _new; \
    _ring->prev = _new; \
  END

/* .ring.insert: */
extern void (RingInsert)(Ring ring, Ring new);
#define RingInsert(ring, new) \
  BEGIN \
    Ring _ring = (ring), _new = (new); \
    AVER(RingCheck(_ring)); \
    AVER(RingCheckSingle(_new)); \
    _new->prev = _ring; \
    _new->next = _ring->next; \
    _ring->next->prev = _new; \
    _ring->next = _new; \
  END

/* .ring.remove: */
extern void (RingRemove)(Ring old);
#define RingRemove(old) \
  BEGIN \
    Ring _old = (old); \
    AVER(RingCheck(_old)); \
    AVER(!RingIsSingle(_old)); \
    _old->next->prev = _old->prev; \
    _old->prev->next = _old->next; \
    _old->next = _old; \
    _old->prev = _old; \
  END

/* .ring.next: */
extern Ring (RingNext)(Ring ring);
#define RingNext(ring)  ((ring)->next)

/* .ring.prev: */
extern Ring (RingPrev)(Ring ring);
#define RingPrev(ring)  ((ring)->prev)

/* .ring.elt: See <design/ring/#elt> */
#define RING_ELT(type, field, node) \
  PARENT(type ## Struct, field, node)

/* .ring.for: See <design/ring/#for> */
#define RING_FOR(node, ring, next) \
  for(node = RingNext(ring), next = RingNext(node); \
      node != (ring); \
      node = (next), next = RingNext(node))


#endif /* ring_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

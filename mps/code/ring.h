/* impl.h.ring: RING INTERFACE
 *
 * $HopeName: MMsrc!ring.h(MMdevel_pekka_locus.1) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 */


#ifndef ring_h
#define ring_h

#include "check.h"
#include "mpmtypes.h"


/* RingStruct -- double-ended queue structure
 *
 * .ring: The ring structure is used as a field in other structures
 * in order to link them together into "rings".  See impl.c.ring.
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

/* .ring.elt: See design.mps.ring.elt */
#define RING_ELT(type, field, node) \
   ((type)((char *)(node) - (size_t)(&((type)0)->field)))

/* .ring.for: See design.mps.ring.for */
#define RING_FOR(node, ring, next) \
  for(node = RingNext(ring), next = RingNext(node); \
      node != (ring); \
      node = (next), next = RingNext(node))


#endif /* ring_h */

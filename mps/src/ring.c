/* impl.c.ring: RING IMPLEMENTATION
 *
 * $HopeName: MMsrc!ring.c(MMdevel_restr2.2) $
 * Copyright (C) 1995 Harlequin Group, all rights reserved.
 *
 * .def: Rings are circular doubly-linked lists of ring "nodes".  The nodes
 * are fields of structures which are the "elements" of the ring.
 *
 * .ex: For example:
 *
 *   typedef struct FooStruct *Foo;     the element type
 *   typedef struct FooStruct {         the element structure
 *     int baz, bim;
 *     RingStruct ring;                 the ring node
 *     float bip, bop;
 *   } FooStruct;
 *
 * .def.singleton: A "singleton" ring is a ring containing one node, whose
 * previous and next nodes are itself.
 *
 * .rationale: Because ring nodes are in-lined in their parent
 * structures the do not need to be managed.  This is especially useful
 * in avoiding re-entrancy and bootstrapping problems in the memory
 * manager.  Rings also provide flexible insertion and deletion because
 * the entire ring can be found from any node.
 *
 * .where.type: The Ring type is defined in impl.h.mpmtypes.
 * .where.struct: The RingStruct structure is defined in impl.h.mpmst.
 */

#include "mpm.h"

SRCID(ring, "$HopeName: MMsrc!ring.c(MMdevel_restr2.2) $");


/* RingCheck, RingCheckSingle -- check the validity of a ring node
 *
 * RingCheck performs a consistency check on the ring node. 
 * RingCheckSingle performs the same check, but also checks that
 * the ring node is a singleton (.def.singleton).
 */

Bool RingCheck(Ring ring)
{
  CHECKL(ring != NULL);
  CHECKL(ring->next != NULL);
  CHECKL(ring->next->prev == ring);
  CHECKL(ring->prev != NULL);
  CHECKL(ring->prev->next == ring);
  return TRUE;
}

Bool RingCheckSingle(Ring ring)
{
  CHECKL(RingCheck(ring));
  CHECKL(ring->next == ring);
  CHECKL(ring->prev == ring);
  return TRUE;
}


/* Ringinit -- initialize a ring node
 *
 * A ring node is intialized to be a singleton (.def.singleton).
 */

void (RingInit)(Ring ring)
{
  RingInit(ring);                       /* impl.h.mpm.init */
}


/* RingFinish -- finish a ring node
 *
 * The ring node must be a singleton to be finished (.def.singleton).
 */
 
void (RingFinish)(Ring ring)
{
  RingFinish(ring);                     /* impl.h.mpm.finish */
}


/* RingAppend -- add a ring node to the end of a ring
 *
 * The "new" node is added immediately previous to the "ring"
 * node, so that it appears at the end of the ring.  The "new" 
 * node must be a singleton (.def.singleton).
 */

void (RingAppend)(Ring ring, Ring new)
{
  RingAppend(ring, new);                /* impl.h.mpm.append */
}


/* RingRemove -- remove a node from a ring
 *
 * The "old" node is removed from the ring it occupies, and becomes
 * a singleton (.def.singleton).  (This has no effect if it is already
 * one.)
 */

void (RingRemove)(Ring old)
{
  RingRemove(old);                      /* impl.h.mpm.remove */
}


/* RingNext -- get the next element of a ring */

Ring (RingNext)(Ring ring)
{
  return RingNext(ring);                /* impl.h.mpm.next */
}


/* RING_ELT -- get the ring element structure
 *
 * _type must be the type of a pointer to the enclosing structure,
 * _field is the name of the ring structure field within it, and
 * _ring is the ring node.  The result is a pointer to the enclosing
 * structure.
 */

/* RING_ELT has no function, and is defined in impl.h.mpm. */


/* RING_FOR -- ring iterator construct
 *
 * This is a for-loop iterator construct for rings.  _var is the
 * iteration variable (of type Ring) to use.  The behaviour is
 * undefined if the ring is modified during the iteration.
 *
 * .ring_for.ex: For example:
 *
 *   Ring ring;
 *   RING_FOR(ring, &foo->barRing) {
 *     Bar bar = RING_ELT(Bar, FooRing, ring);
 *     frob(bar);
 *   }
 */

/* RING_FOR has no function, and is defined in impl.h.mpm. */

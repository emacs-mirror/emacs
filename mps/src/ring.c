/* impl.c.ring: RING IMPLEMENTATION
 *
 * $HopeName: MMsrc!ring.c(trunk.3) $
 * Copyright (C) 1995,1996 Harlequin Group, all rights reserved.
 *
 * .readership: Any MPS developer.
 *
 * .intro: This is a portable implementation of Rings.
 *
 * .purpose: Rings are used to manage potentially unbounded collections
 * of things.
 *
 * .sources: design.mps.ring,
 *   item 6 of mail.richard_brooksby.1996-03-25.16-02
 *
 * .where.type: The Ring type is defined in impl.h.mpmtypes.
 * .where.struct: The RingStruct structure is defined in
 * impl.h.mpmst.ring.
 */


#include "mpm.h"


SRCID(ring, "$HopeName: MMsrc!ring.c(trunk.3) $");


/* RingCheck, RingCheckSingle -- check the validity of a ring node
 *
 * RingCheck performs a consistency check on the ring node. 
 * RingCheckSingle performs the same check, but also checks that
 * the ring node is a singleton (design.mps.ring.def.singleton).
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

Bool RingIsSingle(Ring ring)
{
  AVER(RingCheck(ring));
  if(ring->next == ring) {
    AVER(RingCheckSingle(ring));
    return TRUE;
  }
  return FALSE;
}


/* RingInit -- initialize a ring node
 */

void (RingInit)(Ring ring)
{
  RingInit(ring);                       /* impl.h.mpm.ring.init */
}


/* RingFinish -- finish a ring node
 */
 
void (RingFinish)(Ring ring)
{
  RingFinish(ring);                     /* impl.h.mpm.ring.finish */
}


/* RingAppend -- add a ring node to the end of a ring
 */

void (RingAppend)(Ring ring, Ring new)
{
  RingAppend(ring, new);                /* impl.h.mpm.ring.append */
}


/* RingRemove -- remove a node from a ring
 */

void (RingRemove)(Ring old)
{
  RingRemove(old);                      /* impl.h.mpm.ring.remove */
}


/* RingNext -- get the next element of a ring
 */

Ring (RingNext)(Ring ring)
{
  return RingNext(ring);                /* impl.h.mpm.ring.next */
}


/* RING_ELT -- get the ring element structure
 *
 * RING_ELT has no function (as it does not have function-like
 * behaviour), and is defined in impl.h.mpm.ring.elt.
 */


/* RING_FOR -- ring iterator construct
 *
 * RING_FOR has no function (as it does not have function-like
 * behaviour), and is defined in impl.h.mpm.ring.for.
 */

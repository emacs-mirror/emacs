/* impl.c.ld: LOCATION DEPENDENCY IMPLEMENTATION
 *
 * $HopeName: MMsrc!ld.c(trunk.4) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .def: A location dependency records the fact that the bit-patterns
 * of some references will be used directly (most likely for
 * hashing), and provides a protocol for finding out whether that
 * dependency has become stale because a reference has been changed (by
 * a moving memory manager).
 *
 * .rationale: The client may build hash-tables using pointer hashing.
 * The collector may change the values of the pointers transparently,
 * by fixing them and moving the objects.  The hash function will no
 * longer return the same value, and the object can't be found in
 * the expected bucket.  When the client can't find an object in a
 * hashtable it must check to see if any of the references in the table
 * have moved, and rehash if they have.  Location dependency provides
 * a reasonably accurate way of determining whether this has happened.
 *
 * .impl: A location dependency consists of an epoch (monotonically
 * increasing notion of time) and a reference set.  The epoch records
 * when the location dependency started, and the reference set
 * accumulates an approximation to the set of references which are
 * depended on.  The client can check to see if any of these
 * references have moved since the epoch.
 *
 * .history: The current epoch, and a history of object movement
 * are recorded in the space.  Each slot in the history contains a
 * summary of all the movement since an earlier epoch (maintained by
 * LDAge).  To see if a dependency has become stale all that
 * is needed is to see whether its reference set intersects with the
 * movement since its epoch.
 *
 * .mod: SPACE_LD_LENGTH is used as a modulus to calculate the offset
 * of an epoch in the history, so it's best if this is a power of two.
 * (impl.h.mpmconf)
 *
 * .epoch-size: The epoch should probably be a longer integer to avoid
 * the possibility of overflow.
 * (32 bits only gives 50 days at 1ms frequency)
 *
 * .ld.access: Accesses (reads and writes) to the ld structure must be
 * "wrapped" with an ShieldExpose/Cover pair if and only if the access
 * is taking place inside the space.  Currently this is only the case for
 * LDReset.
 */

#include "mpm.h"

SRCID(ld, "$HopeName: MMsrc!ld.c(trunk.4) $");


/* LDReset -- reset a dependency to empty
 *
 * .reset.sync: This does not need to be synchronized with LDAge
 * because if the epoch advances after it is read the dependency
 * will simply include movement for more time than necessary.
 */

void LDReset(LD ld, Space space)
{
  Bool b;
  Seg seg;

  AVER(ld != NULL);
  AVERT(Space, space);

  b = SegOfAddr(&seg, space, (Addr)ld);
  if(b) {
    ShieldExpose(space, seg);   /* .ld.access */
  }
  ld->epoch = space->epoch;
  ld->rs = RefSetEMPTY;
  if(b) {
    ShieldCover(space, seg);
  }
}


/* LDAdd -- add a reference to a dependency
 *
 * .add.lock-free:  This function is thread safe with respect to the
 * (rest of the) mps.  It is unnecessary to claim locks before calling
 * this function.
 *
 * .add.user-serial:
 * However, this function is _not_ thread safe with respect to itself.
 * Users should ensure that calls to LDAdd operating on the same LD are
 * serialized.
 *
 * .add.sync: Add must take place _before_ the location of the
 * reference is depended on.  If the reference changes between
 * adding and depending it will show up as moved because the
 * movement will have occured since the epoch recorded in the
 * dependency.  If the location were used first only the new
 * location of the reference would end up in the set.
 */

void LDAdd(LD ld, Space space, Addr addr)
{
  AVER(ld->epoch <= space->epoch);
  /* .add.lock-free
   * AVERT(Space, space) */

  ld->rs = RefSetAdd(space, ld->rs, addr);
}


/* LDIsStale -- check whether a dependency is stale
 *
 * .stale.thread-safe: This function is thread safe.  It will return a
 * correct (but possibly conservative) answer regardless of the number
 * of calls to LDAge anywhere during the function. Update with care.
 *
 * .stale.current: If the dependency's epoch is the current epoch,
 * nothing can have moved since it was initialized.
 *
 * .stale.recent: If the dependency is recent, see if it intersects
 * with everything which has moved since it was initialized.
 *
 * .stale.recent.conservative: The refset from the history table is
 * loaded before we check whether ld->epoch is "recent" with respect to
 * the current epoch.  This means that we may (conservatively) decide
 * to use the prehistory instead.
 *
 * .stale.old: Otherwise, if the dependency is older than the length
 * of the history, check it against all movement that has ever occured.
 */

Bool LDIsStale(LD ld, Space space, Addr addr)
{
  RefSet rs;

  UNUSED(addr);

  AVER(ld->epoch <= space->epoch);
  /* .stale.thread-safe
   * AVERT(Space, space) */

  if(space->epoch == ld->epoch) /* .stale.current */
    return FALSE;

  /* Load the history refset, _then_ check to see if it's recent.
   * This may in fact load an okay refset, which we decide to throw
   * away and use the pre-history instead. */
  rs = space->history[ld->epoch % SPACE_LD_LENGTH];
  /* .stale.recent */
  /* .stale.recent.conservative */
  if(space->epoch - ld->epoch > SPACE_LD_LENGTH) {
    rs = space->prehistory;     /* .stale.old */
  }

  return RefSetInter(ld->rs, rs) != RefSetEMPTY;
}


/* LDAge -- age the space by adding a moved set
 *
 * This stores the fact that a set of references has changed in
 * the history in the space structure, and increments the epoch.
 */

void LDAge(Space space, RefSet rs)
{
  Size i;

  AVERT(Space, space);
  AVER(rs != RefSetEMPTY);

  /* Replace the entry for epoch - SPACE_LD_LENGTH by an empty */
  /* set which will become the set which has moved since the */
  /* current epoch. */
  space->history[space->epoch % SPACE_LD_LENGTH] = RefSetEMPTY;

  /* Record the fact that the moved set has moved, by adding it */
  /* to all the sets in the history, including the set for the */
  /* current epoch. */
  for(i = 0; i < SPACE_LD_LENGTH; ++i)
    space->history[i] = RefSetUnion(space->history[i], rs);

  /* This is the union of all movement since time zero. */
  space->prehistory = RefSetUnion(space->prehistory, rs);

  /* Advance the epoch by one. */
  ++space->epoch;
  AVER(space->epoch != 0);      /* .epoch-size */
}

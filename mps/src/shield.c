/* impl.c.shield: SHIELD IMPLEMENTATION
 *
 * $HopeName: MMsrc!shield.c(MMdevel_restr.6) $
 *
 * See: idea.shield, design.mps.shield.
 *
 * This implementation of the shield avoids suspending threads for
 * as long as possible.  When threads are suspended, it maintains a
 * cache of covered segments where the desired and actual protection
 * do not match.  This cache is flushed on leaving the shield.
 *
 * Definitions
 *
 * .def.synced: a seg is synced if the prot and shield modes are the
 * same, and unsynced otherwise.
 * .def.depth: the depth of a segment is defined as
 *   depth == #exposes - #covers + I(in cache),  where
 *     #exposes = the total number of times the seg has been exposed
 *     #covers  = the total number of times the seg has been covered
 *     I(in cache) = 1 if the segment is in the cache
 *                   0 otherwise
 *   The cache is initially empty and cover should not be called
 *   without a matching expose, so this figure should always be
 *   non-negative.
 * .def.total.depth: The total depth is the sum of the depth over
 * all segments
 * .def.outside: being outside the shield is being between calls
 * to leave and enter, and similarly .def.inside: being inside the
 * shield is being between calls to enter and leave.
 * .def.suspended: suspended is true iff the threads are suspended
 * .def.exposed: a segment is exposed if #exposes > #covers and
 * covered otherwise.
 *
 * Properties
 *
 * .prop.outside.running: The mutator may not be suspended while
 * outside the shield.
 * .prop.mutator.access: An attempt by the mutator to access
 * shielded memory must cause a SpaceAccess.
 * .prop.inside.access: Inside the shield it must be possible to access
 * all unshielded segments and all exposed segments.
 *
 * Invariants
 *
 * These invariants are maintained by the code.
 *
 * .inv.outside.running: The mutator is running while outside the
 * shield.
 * .inv.unsynced.suspended: If any segment is not synced,
 *  the mutator is suspended.
 * .inv.unsynced.depth: All unsynced segments have positive depth.
 * .inv.outside.depth: The total depth is zero while outside the shield.
 * .inv.prot.shield: The prot mode is never more than the shield mode.
 * .inv.expose.prot: An exposed seg is not protected.
 *
 * Hints at proofs of properties from invariants
 *
 * inv.outside.running directly ensures prop.outside running.
 *
 * As the depth of a segment cannot be negative
 *   total depth == 0 => for all segments, depth == 0
 *                    => all segs are synced (by .inv.unsynced.depth)
 * 
 * If the mutator is running then all segs must be synced
 * (.inv.unsynced.suspend).  Which means that the hardware protection
 * (prot mode) must reflect the software protection (shield mode).
 * Hence all shielded memory will be hardware protected while the
 * mutator is running.  This ensures .prop.mutator.access.
 *
 * inv.prot.shield and inv.expose.prot ensure prop.inside.access.
 */

#include "mpm.h"

SRCID(shield, "$HopeName: MMsrc!shield.c(MMdevel_restr.6) $");

void ShieldSuspend(Space space)
{
  AVERT(Space, space);
  AVER(space->insideShield);

  if(!space->suspended) {
    ThreadRingSuspend(SpaceThreadRing(space));
    space->suspended = TRUE;
  }
}

void ShieldResume(Space space)
{
  AVERT(Space, space);
  AVER(space->insideShield);
  AVER(space->suspended);
  /* It is only correct to actually resume the mutator here if 
   * shDepth is 0
   */
}

/* This ensures actual prot mode does not include mode */
static void protLower(Space space, Seg seg, AccessSet mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  if(seg->pm & mode) {
    seg->pm &= ~mode;
    ProtSet(SegBase(space, seg), SegLimit(space, seg), seg->pm);
  }
}

static void sync(Space space, Seg seg)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  if(seg->pm != seg->sm) {
    ProtSet(SegBase(space, seg), SegLimit(space, seg), seg->sm);
    seg->pm = seg->sm;
    /* inv.prot.shield */
  }
}

static void flush(Space space, Size i)
{
  Seg seg;
  AVERT(Space, space);
  AVER(i < SHIELD_CACHE_SIZE);

  seg = space->shCache[i];
  if(seg == (Seg)0) return;
  AVERT(Seg, seg);

  AVER(space->shDepth > 0);
  AVER(seg->depth > 0);
  --space->shDepth;
  --seg->depth;
  
  if(seg->depth == 0)
    sync(space, seg);

  space->shCache[i] = (Seg)0;
}

/* If the segment is out of sync, either sync it, or ensure
 * depth > 0, and the space is suspended.
 */
static void cache(Space space, Seg seg)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  if(seg->sm == seg->pm) return;
  if(seg->depth > 0) {
    ShieldSuspend(space);
    return;
  }
  if(SHIELD_CACHE_SIZE == 0 || !space->suspended)
    sync(space, seg);
  else {
    ++seg->depth;
    ++space->shDepth;
    AVER(space->shDepth > 0);
    AVER(seg->depth > 0);
    AVER(space->shCacheI < SHIELD_CACHE_SIZE);
    flush(space, space->shCacheI);
    space->shCache[space->shCacheI] = seg;
    ++space->shCacheI;
    if(space->shCacheI == SHIELD_CACHE_SIZE)
      space->shCacheI = 0;
  }
}

void ShieldRaise(Space space, Seg seg, AccessSet mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  AVER((seg->sm & mode) == AccessSetEMPTY);
  seg->sm |= mode; /* inv.prot.shield preserved */

  /* ensure inv.unsynced.suspended & inv.unsynced.depth */
  cache(space, seg);
}

void ShieldLower(Space space, Seg seg, AccessSet mode)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  AVER((seg->sm & mode) == mode);
  /* synced(seg) is not changed by the following
   * preserving inv.unsynced.suspended
   * Also inv.prot.shield preserved
   */
  seg->sm &= ~mode;
  protLower(space, seg, mode);
}

void ShieldEnter(Space space)
{
  Size i;

  AVERT(Space, space);
  AVER(!space->insideShield);
  AVER(space->shDepth == 0);
  AVER(!space->suspended);
  AVER(space->shCacheI < SHIELD_CACHE_SIZE);
  for(i = 0; i < SHIELD_CACHE_SIZE; i++)
    AVER(space->shCache[i] == (Seg)0);

  space->insideShield = TRUE;
}

/* Flush empties the shield cache.
 * This needs to be called before segments are destroyed as there
 * may be references to them in the cache.
 */
void ShieldFlush(Space space)
{
  Size i;

  for(i = 0; i < SHIELD_CACHE_SIZE; ++i) {
    if(space->shDepth == 0)
      break;
    flush(space, i);
  }
}

void ShieldLeave(Space space)
{
  AVERT(Space, space);
  AVER(space->insideShield);

  ShieldFlush(space);
  /* Cache is empty so inv.outside.depth holds */
  AVER(space->shDepth == 0);

  /* Ensuring the mutator is running at this point
   * guarantees inv.outside.running */
  if(space->suspended) {
    ThreadRingResume(SpaceThreadRing(space));
    space->suspended = FALSE;
  }
  space->insideShield = FALSE;
}


void ShieldExpose(Space space, Seg seg)
{
  AccessSet mode = AccessREAD | AccessWRITE;
  AVERT(Space, space);
  AVER(space->insideShield);

  ++seg->depth;
  ++space->shDepth;
  AVER(space->shDepth > 0);
  AVER(seg->depth > 0);
  if(seg->pm & mode)
    ShieldSuspend(space);

  /* This ensures inv.expose.prot */
  protLower(space, seg, mode);
}

void ShieldCover(Space space, Seg seg)
{
  AVERT(Space, space);
  AVERT(Seg, seg);
  AVER(seg->pm == AccessSetEMPTY);

  AVER(space->shDepth > 0);
  AVER(seg->depth > 0);
  --seg->depth;
  --space->shDepth;

  /* ensure inv.unsynced.depth */
  cache(space, seg);
}

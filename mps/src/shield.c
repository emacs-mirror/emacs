/* impl.c.shield: SHIELD IMPLEMENTATION
 *
 * $HopeName: MMsrc!shield.c(trunk.15) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 *
 * See: idea.shield, design.mps.shield.
 *
 * This implementation of the shield avoids suspending threads for
 * as long as possible.  When threads are suspended, it maintains a
 * cache of covered segments where the desired and actual protection
 * do not match.  This cache is flushed on leaving the shield.
 *
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
 *
 *
 * Properties
 *
 * .prop.outside.running: The mutator may not be suspended while
 * outside the shield.
 * .prop.mutator.access: An attempt by the mutator to access
 * shielded memory must cause an ArenaAccess.
 * .prop.inside.access: Inside the shield it must be possible to access
 * all unshielded segments and all exposed segments.
 *
 *
 * Invariants
 *
 * These invariants are maintained by the code.
 *
 * .inv.outside.running: The mutator is running while outside the
 * shield.
 * .inv.unsynced.suspended: If any segment is not synced,
 * the mutator is suspended.
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

SRCID(shield, "$HopeName: MMsrc!shield.c(trunk.15) $");


void (ShieldSuspend)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  if (!arena->suspended) {
    ThreadRingSuspend(ArenaThreadRing(arena));
    arena->suspended = TRUE;
  }
}


void (ShieldResume)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);
  AVER(arena->suspended);
  /* It is only correct to actually resume the mutator here if shDepth is 0 */
}


/* This ensures actual prot mode does not include mode */
static void protLower(Arena arena, Seg seg, AccessSet mode)
{
  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Arena, arena);
  UNUSED(arena);
  AVERT_CRITICAL(Seg, seg);

  if (SegPM(seg) & mode) {
    SegSetPM(seg, SegPM(seg) & ~mode);
    ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg));
  }
}


static void sync(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);

  if (SegPM(seg) != SegSM(seg)) {
    ProtSet(SegBase(seg), SegLimit(seg), SegSM(seg));
    SegSetPM(seg, SegSM(seg));
    /* inv.prot.shield */
  }
}


static void flush(Arena arena, Size i)
{
  Seg seg;
  AVERT(Arena, arena);
  AVER(i < arena->shCacheLimit);

  seg = arena->shCache[i];
  if (seg == NULL) return;
  AVERT(Seg, seg);

  AVER(arena->shDepth > 0);
  AVER(SegDepth(seg) > 0);
  --arena->shDepth;
  SegSetDepth(seg, SegDepth(seg) - 1);
  
  if (SegDepth(seg) == 0)
    sync(arena, seg);

  arena->shCache[i] = NULL;
}


/* If the segment is out of sync, either sync it, or ensure
 * depth > 0, and the arena is suspended.
 */
static void cache(Arena arena, Seg seg)
{
  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Seg, seg);

  if (SegSM(seg) == SegPM(seg)) return;
  if (SegDepth(seg) > 0) {
    ShieldSuspend(arena);
    return;
  }
  if (ShieldCacheSIZE == 0 || !arena->suspended)
    sync(arena, seg);
  else {
    SegSetDepth(seg, SegDepth(seg) + 1);
    ++arena->shDepth;
    AVER(arena->shDepth > 0);
    AVER(SegDepth(seg) > 0);
    AVER(arena->shCacheLimit <= ShieldCacheSIZE);
    AVER(arena->shCacheI < arena->shCacheLimit);
    flush(arena, arena->shCacheI);
    arena->shCache[arena->shCacheI] = seg;
    ++arena->shCacheI;
    if (arena->shCacheI == ShieldCacheSIZE)
      arena->shCacheI = 0;
    if (arena->shCacheI == arena->shCacheLimit)
      ++arena->shCacheLimit;
  }
}


void (ShieldRaise) (Arena arena, Seg seg, AccessSet mode)
{
  /* .seg.broken: Seg's shield invariants may not be true at */
  /* this point (this function is called to enforce them) so we */
  /* can't check seg. Nor can we check arena as that checks the */
  /* segs in the cache. */

  AVER((SegSM(seg) & mode) == AccessSetEMPTY);
  SegSetSM(seg, SegSM(seg) | mode); /* inv.prot.shield preserved */

  /* ensure inv.unsynced.suspended & inv.unsynced.depth */
  cache(arena, seg);
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


void (ShieldLower)(Arena arena, Seg seg, AccessSet mode)
{
  /* Don't check seg or arena, see .seg.broken */
  AVER((SegSM(seg) & mode) == mode);
  /* synced(seg) is not changed by the following
   * preserving inv.unsynced.suspended
   * Also inv.prot.shield preserved
   */
  SegSetSM(seg, SegSM(seg) & ~mode);
  protLower(arena, seg, mode);
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


void (ShieldEnter)(Arena arena)
{
  Size i;

  AVERT(Arena, arena);
  AVER(!arena->insideShield);
  AVER(arena->shDepth == 0);
  AVER(!arena->suspended);
  AVER(arena->shCacheLimit <= ShieldCacheSIZE);
  AVER(arena->shCacheI < arena->shCacheLimit);
  for(i = 0; i < arena->shCacheLimit; i++)
    AVER(arena->shCache[i] == NULL);

  arena->shCacheI = (Size)0;
  arena->shCacheLimit = (Size)1;
  arena->insideShield = TRUE;
}


/* .shield.flush: Flush empties the shield cache.
 * This needs to be called before segments are destroyed as there
 * may be references to them in the cache.
 */
void (ShieldFlush)(Arena arena)
{
  Size i;

  for(i = 0; i < arena->shCacheLimit; ++i) {
    if (arena->shDepth == 0)
      break;
    flush(arena, i);
  }
}


void (ShieldLeave)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  ShieldFlush(arena);
  /* Cache is empty so inv.outside.depth holds */
  AVER(arena->shDepth == 0);

  /* Ensuring the mutator is running at this point
   * guarantees inv.outside.running */
  if (arena->suspended) {
    ThreadRingResume(ArenaThreadRing(arena));
    arena->suspended = FALSE;
  }
  arena->insideShield = FALSE;
}


void (ShieldExpose)(Arena arena, Seg seg)
{
  AccessSet mode = AccessREAD | AccessWRITE;
  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Arena, arena);
  AVER_CRITICAL(arena->insideShield);

  SegSetDepth(seg, SegDepth(seg) + 1);
  ++arena->shDepth;
  /* design.mps.trace.fix.noaver */
  AVER_CRITICAL(arena->shDepth > 0);
  AVER_CRITICAL(SegDepth(seg) > 0);
  if (SegPM(seg) & mode)
    ShieldSuspend(arena);

  /* This ensures inv.expose.prot */
  protLower(arena, seg, mode);
}


void (ShieldCover)(Arena arena, Seg seg)
{
  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(SegPM(seg) == AccessSetEMPTY);

  AVER_CRITICAL(arena->shDepth > 0);
  AVER_CRITICAL(SegDepth(seg) > 0);
  SegSetDepth(seg, SegDepth(seg) - 1);
  --arena->shDepth;

  /* ensure inv.unsynced.depth */
  cache(arena, seg);
}

/* shield.c: SHIELD IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
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
 *   depth == #exposes - #covers + #(in cache),  where
 *     #exposes = the total number of times the seg has been exposed
 *     #covers  = the total number of times the seg has been covered
 *     #(in cache) = the number of times the seg appears in the cache
 *   The cache is initially empty and Cover should not be called
 *   without a matching Expose, so this figure should always be
 *   non-negative.
 * .def.total.depth: The total depth is the sum of the depth over
 * all segments
 * .def.outside: being outside the shield is being between calls
 * to leave and enter, and similarly .def.inside: being inside the
 * shield is being between calls to enter and leave.
 * .def.suspended: suspended is true iff the mutator is suspended.
 * .def.shielded: a segment is shielded if the shield mode is non-zero.
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
 * .inv.outside.running: The mutator is not suspended while outside the
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

SRCID(shield, "$Id$");


/* shieldSegIsSynced -- is a segment synced?
 *
 * See .def.synced.
 */

static Bool shieldSegIsSynced(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return SegSM(seg) == SegPM(seg);
}


/* shieldSync -- synchronize a segment's protection */

static void shieldSync(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);

  if (!shieldSegIsSynced(seg)) {
    ProtSet(SegBase(seg), SegLimit(seg), SegSM(seg));
    SegSetPM(seg, SegSM(seg));
    /* See .inv.prot.shield. */
  }
}


/* ShieldSuspend -- suspend the mutator
 *
 * From outside impl.c.shield, this is used when we really need to
 * lock everything against the mutator -- for example, during flip
 * when we must scan all thread registers at once.
 *
 * It is called from inside impl.c.shield when any segment is not
 * synced -- see .inv.unsynced.suspended.
 */

void (ShieldSuspend)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  if (!arena->suspended) {
    ThreadRingSuspend(ArenaThreadRing(arena), ArenaDeadRing(arena));
    arena->suspended = TRUE;
  }
}


/* ShieldResume -- declare mutator could be resumed
 *
 * In practice, we don't resume the mutator until ShieldLeave, but
 * this marks the earliest point at which we could resume.
 */

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
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  UNUSED(arena);
  AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(AccessSet, mode);

  if (SegPM(seg) & mode) {
    SegSetPM(seg, SegPM(seg) & ~mode);
    ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg));
  }
}


static void shieldFlushEntry(Arena arena, Size i)
{
  Seg seg;
  AVERT(Arena, arena);
  AVER(i < arena->shCacheLimit);

  seg = arena->shCache[i];
  AVERT(Seg, seg);

  AVER(arena->shDepth > 0);
  AVER(SegDepth(seg) > 0);
  --arena->shDepth;
  SegSetDepth(seg, SegDepth(seg) - 1);

  if (SegDepth(seg) == 0)
    shieldSync(arena, seg);

  arena->shCache[i] = NULL; /* just to make sure it gets overwritten */
}


static Compare shieldCacheEntryCompare(void *left, void *right, void *closure)
{
  Seg segA = left, segB = right;
  Addr baseA, baseB;

  /* Making these CRITICAL had no effect on timings on LII6LL today.
     RB 2016-03-17. */
  AVERT(Seg, segA);
  AVERT(Seg, segB);
  UNUSED(closure);

  baseA = SegBase(segA);
  baseB = SegBase(segB);
  if (baseA < baseB)
    return CompareLESS;
  if (baseA > baseB)
    return CompareGREATER;
  AVER(baseA == baseB);
  return CompareEQUAL;
}


static void shieldCacheReset(Arena arena)
{
  AVER(arena->shDepth == 0);
  arena->shCacheI = 0;
  arena->shCacheLimit = 0;
}


/* shieldFlushEntries -- flush cache coalescing protects
 *
 * base, limit and mode represent outstanding protection to be done.
 */

static void shieldFlushEntries(Arena arena)
{
  Addr base = 0, limit = 0;
  AccessSet mode = 0;
  Seg seg;
  Size i;
  if (arena->shDepth == 0)
    return;
  AVER(arena->shCache != NULL);
  AVER(arena->shCacheLength > 0);

  QuickSort((void *)arena->shCache, arena->shCacheLimit,
            shieldCacheEntryCompare, UNUSED_POINTER);

  seg = arena->shCache[0]; /* lowest address segment */
  if (seg) {
    AVERT(Seg, seg);
    arena->shCache[0] = NULL; /* just to make sure it isn't reused */

    AVER(arena->shDepth > 0);
    AVER(SegDepth(seg) > 0);
    --arena->shDepth;
    SegSetDepth(seg, SegDepth(seg) - 1);

    if (!shieldSegIsSynced(seg)) {
      SegSetPM(seg, SegSM(seg));
      base = SegBase(seg);
      limit = SegLimit(seg);
      mode = SegSM(seg);
    }
  }
  for (i=1; i < arena->shCacheLimit; ++i) {
    if (arena->shDepth == 0)
      break;
    seg = arena->shCache[i];
    AVERT(Seg, seg);
    arena->shCache[i] = NULL; /* just to make sure it isn't reused */

    AVER(arena->shDepth > 0);
    AVER(SegDepth(seg) > 0);
    --arena->shDepth;
    SegSetDepth(seg, SegDepth(seg) - 1);

    if (!shieldSegIsSynced(seg)) {
      SegSetPM(seg, SegSM(seg));
      if (SegBase(seg) != limit || mode != SegSM(seg)) {
        if (limit != 0) {
          ProtSet(base, limit, mode);
        }
        base = SegBase(seg);
        limit = SegLimit(seg);
        mode = SegSM(seg);
      } else {
        limit = SegLimit(seg);
      }
    }
  }
  if (limit != 0) {
    ProtSet(base, limit, mode);
  }
  shieldCacheReset(arena);
}


/* shieldCache -- consider adding a segment to the cache
 *
 * If the segment is out of sync, either sync it, or ensure depth > 0,
 * and the arena is suspended.
 */

static void shieldCache(Arena arena, Seg seg)
{
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Seg, seg);

  if (shieldSegIsSynced(seg))
    return;
  
  if (SegDepth(seg) > 0) { /* already in the cache or exposed? */
    /* This can occur if the mutator isn't suspended, we expose a
       segment, then raise the shield on it.  In this case, the
       mutator isn't allowed to see the segment, but we don't need to
       cache it until its covered. */
    ShieldSuspend(arena);
    return;
  }

  /* Allocate shield cache if necessary. */
  /* FIXME: This will try to extend the cache on every attempt, even
     if it failed last time. That might be slow. */
  if (arena->shCacheI >= arena->shCacheLength) {
    void *p;
    Res res;
    Count length;

    AVER(arena->shCacheI == arena->shCacheLength);

    if (arena->shCacheLength == 0)
      length = ShieldCacheSIZE;
    else
      length = arena->shCacheLength * 2;
    
    res = ControlAlloc(&p, arena, length * sizeof arena->shCache[0]);
    if (res != ResOK) {
      AVER(ResIsAllocFailure(res));
      /* Carry on with the existing cache. */
    } else {
      if (arena->shCacheLength > 0) {
        Size oldSize = arena->shCacheLength * sizeof arena->shCache[0];
        AVER(arena->shCache != NULL);
        mps_lib_memcpy(p, arena->shCache, oldSize);
        ControlFree(arena, arena->shCache, oldSize);
      }
      arena->shCache = p;
      arena->shCacheLength = length;
    }
  }

  /* Cache unavailable, so synchronize now.  Or if the mutator is not
     yet suspended and the code raises the shield on a covered
     segment, protect it now, because that's probably better than
     suspending the mutator. */
  if (arena->shCacheLength == 0 || !arena->suspended) {
    shieldSync(arena, seg);
    return;
  }

  SegSetDepth(seg, SegDepth(seg) + 1);
  AVER_CRITICAL(SegDepth(seg) > 0); /* overflow */
  ++arena->shDepth;
  AVER_CRITICAL(arena->shDepth > 0); /* overflow */

  AVER_CRITICAL(arena->shCacheLimit <= arena->shCacheLength);
  AVER_CRITICAL(arena->shCacheI <= arena->shCacheLimit);

  if (arena->shCacheI >= arena->shCacheLength)
    arena->shCacheI = 0;
  AVER_CRITICAL(arena->shCacheI < arena->shCacheLength);

  AVER_CRITICAL(arena->shCacheLength > 0);

  /* If the limit is less than the length, then the cache array has
     yet to be filled, and shCacheI is an uninitialized entry.
     Otherwise it's the tail end from last time around, and needs to
     be flushed. */
  if (arena->shCacheLimit == arena->shCacheLength)
    shieldFlushEntry(arena, arena->shCacheI);

  arena->shCache[arena->shCacheI] = seg;
  ++arena->shCacheI;

  if (arena->shCacheI >= arena->shCacheLimit)
    arena->shCacheLimit = arena->shCacheI;
}


void (ShieldRaise) (Arena arena, Seg seg, AccessSet mode)
{
  /* .seg.broken: Seg's shield invariants may not be true at */
  /* this point (this function is called to enforce them) so we */
  /* can't check seg. Nor can we check arena as that checks the */
  /* segs in the cache. */

  AVERT(AccessSet, mode);
  AVER((SegSM(seg) & mode) == AccessSetEMPTY);
  SegSetSM(seg, SegSM(seg) | mode); /* inv.prot.shield preserved */

  /* ensure inv.unsynced.suspended & inv.unsynced.depth */
  shieldCache(arena, seg);
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


void (ShieldLower)(Arena arena, Seg seg, AccessSet mode)
{
  /* Don't check seg or arena, see .seg.broken */
  AVERT(AccessSet, mode);
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
  AVERT(Arena, arena);
  AVER(!arena->insideShield);
  AVER(arena->shDepth == 0);
  AVER(!arena->suspended);
  AVER(arena->shCacheLimit <= arena->shCacheLength);
  AVER(arena->shCacheI <= arena->shCacheLimit);

  shieldCacheReset(arena);

  arena->insideShield = TRUE;
}


/* .shield.flush: Flush empties the shield cache.
 * This needs to be called before segments are destroyed as there
 * may be references to them in the cache.
 *
 * The memory for the segment may become spare, and not released back to
 * the operating system. Since we keep track of protection on segments
 * and not grains we have no way of keeping track of the protection
 * state of spare grains. We therefore flush the protection to get it
 * back into the default state (unprotected).
 */
void (ShieldFlush)(Arena arena)
{
  Size i;

  if(1)
    shieldFlushEntries(arena);

  for (i = 0; i < arena->shCacheLimit; ++i) {
    if (arena->shDepth == 0)
      break;
    shieldFlushEntry(arena, i);
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
    ThreadRingResume(ArenaThreadRing(arena), ArenaDeadRing(arena));
    arena->suspended = FALSE;
  }
  arena->insideShield = FALSE;
}


/* ShieldExpose -- allow the MPS access to a segment while denying the mutator
 *
 * The MPS currently does not collect concurrently, however the only thing
 * that makes it not-concurrent is a critical point in the Shield
 * abstraction where the MPS seeks to gain privileged access to memory
 * (usually in order to scan it for GC). The critical point is where
 * ShieldExpose in shield.c has to call ShieldSuspend to preserve the
 * shield invariants. This is the only point in the MPS that prevents
 * concurrency, and the rest of the MPS is designed to support it.
 *
 * The restriction could be removed if either:
 * 
 *  * the MPS could use a different set of protections to the mutator
 *   program
 * 
 *  * the mutator program uses a software barrier
 * 
 * The first one is tricky, and the second one just hasn't come up in any
 * implementation we've been asked to make yet. Given a VM, it could
 * happen, and the MPS would be concurrent.
 * 
 * So, I believe there's nothing fundamentally non-concurrent about the
 * MPS design. It's kind of waiting to happen.
 *
 * (Originally written at <http://news.ycombinator.com/item?id=4524036>.)
 */

void (ShieldExpose)(Arena arena, Seg seg)
{
  AccessSet mode = AccessREAD | AccessWRITE;
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  AVER_CRITICAL(arena->insideShield);

  SegSetDepth(seg, SegDepth(seg) + 1);
  ++arena->shDepth;
  /* <design/trace/#fix.noaver> */
  AVER_CRITICAL(arena->shDepth > 0);
  AVER_CRITICAL(SegDepth(seg) > 0);
  if (SegPM(seg) & mode)
    ShieldSuspend(arena);

  /* This ensures inv.expose.prot */
  protLower(arena, seg, mode);
}


void (ShieldCover)(Arena arena, Seg seg)
{
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(SegPM(seg) == AccessSetEMPTY);

  AVER_CRITICAL(arena->shDepth > 0);
  AVER_CRITICAL(SegDepth(seg) > 0);
  SegSetDepth(seg, SegDepth(seg) - 1);
  --arena->shDepth;

  /* ensure inv.unsynced.depth */
  shieldCache(arena, seg);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

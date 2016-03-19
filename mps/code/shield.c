/* shield.c: SHIELD IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * See: idea.shield, design.mps.shield.
 *
 * IMPORTANT: This code is subtle and critical. Ensure you have read
 * and understood design.mps.shield before you touch it.
 */

#include "mpm.h"

SRCID(shield, "$Id$");


/* SHIELD_AVER -- transgressive argument checking
 *
 * .trans.check: A number of shield functions cannot do normal
 * argument checking with AVERT because (for example) SegCheck checks
 * the shield invariants, and it is these functions that are enforcing
 * them.  Instead, we AVER(TESTT(Seg, seg)) to check the type
 * signature but not the contents.
 */

#define SHIELD_AVERT(type, exp) AVER(TESTT(type, exp))
#define SHIELD_AVERT_CRITICAL(type, exp) AVER_CRITICAL(TESTT(type, exp))


/* SegIsSynced -- is a segment synced?
 *
 * See design.mps.shield.def.synced.
 */

static Bool SegIsSynced(Seg seg)
{
  SHIELD_AVERT_CRITICAL(Seg, seg);
  return SegSM(seg) == SegPM(seg);
}


/* SegIsExposed -- is a segment exposed?
 *
 * See design.mps.shield.def.exposed.
 */

static Bool SegIsExposed(Seg seg)
{
  SHIELD_AVERT_CRITICAL(Seg, seg);
  return seg->depth > 0;
}


/* shieldSync -- synchronize a segment's protection
 *
 * See design.mps.shield.inv.prot.shield.
 */

static void shieldSync(Arena arena, Seg seg)
{
  AVERT(Arena, arena);
  SHIELD_AVERT_CRITICAL(Seg, seg);

  if (!SegIsSynced(seg)) {
    ProtSet(SegBase(seg), SegLimit(seg), SegSM(seg));
    SegSetPM(seg, SegSM(seg));
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
  /* TODO: Consider actually doing that. */
}


/* shieldProtLower -- reduce protection on a segment
 *
 * This ensures actual prot mode does not include mode.
 */

static void shieldProtLower(Arena arena, Seg seg, AccessSet mode)
{
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  UNUSED(arena);
  SHIELD_AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(AccessSet, mode);

  if (BS_INTER(SegPM(seg), mode) != AccessSetEMPTY) {
    SegSetPM(seg, BS_DIFF(SegPM(seg), mode));
    ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg));
  }
}


/* shieldDecache -- remove a segment from the shield cache */

static Seg shieldDecache(Arena arena, Index i)
{
  Seg seg;
  AVER(i < arena->shCacheLimit);
  seg = arena->shCache[i];
  AVERT(Seg, seg);
  AVER(seg->cached);
  arena->shCache[i] = NULL;
  seg->cached = FALSE;
  return seg;
}


/* shieldFlushEntry -- flush a single entry from the cache */

static void shieldFlushEntry(Arena arena, Index i)
{
  Seg seg = shieldDecache(arena, i);

  if (!SegIsExposed(seg))
    shieldSync(arena, seg);
}


/* shieldCacheReset -- reset shield cache pointers */

static void shieldCacheReset(Arena arena)
{
  AVER(arena->shDepth == 0); /* overkill: implies no segs are cached */
  arena->shCacheI = 0;
  arena->shCacheLimit = 0;
}


/* shieldCacheEntryCompare -- comparison for cache sorting */

static Compare shieldAddrCompare(Addr left, Addr right)
{
  if (left < right)
    return CompareLESS;
  else if (left == right)
    return CompareEQUAL;
  else
    return CompareGREATER;
}

static Compare shieldCacheEntryCompare(void *left, void *right, void *closure)
{
  Seg segA = left, segB = right;

  /* These checks are not critical in a hot build, but slow down cool
     builds quite a bit, so just check the signatures. */
  AVER(TESTT(Seg, segA));
  AVER(TESTT(Seg, segB));
  UNUSED(closure);

  return shieldAddrCompare(SegBase(segA), SegBase(segB));
}


/* shieldFlushEntries -- flush cache coalescing protects
 *
 * Sort the shield cache into address order, then iterate over it
 * coalescing protection work, in order to reduce the number of system
 * calls to a minimum.  This is very important on OS X, where
 * protection calls are extremely inefficient, but has no net gain on
 * Windows.
 *
 * base, limit and mode represent outstanding protection to be done.
 */

static void shieldFlushEntries(Arena arena)
{
  Addr base = NULL, limit = NULL;
  AccessSet mode;
  Index i;

  if (arena->shCacheLength == 0) {
    AVER(arena->shCache == NULL);
    return;
  }

  QuickSort((void *)arena->shCache, arena->shCacheLimit,
            shieldCacheEntryCompare, UNUSED_POINTER);

  mode = AccessSetEMPTY;
  for (i = 0; i < arena->shCacheLimit; ++i) {
    Seg seg = shieldDecache(arena, i);
    if (!SegIsSynced(seg)) {
      AVER(SegSM(seg) != AccessSetEMPTY); /* can't match first iter */
      SegSetPM(seg, SegSM(seg));
      if (SegSM(seg) != mode || SegBase(seg) != limit) {
        if (mode != AccessSetEMPTY) {
          AVER(base != NULL);
          AVER(base < limit);
          ProtSet(base, limit, mode);
        }
        base = SegBase(seg);
        mode = SegSM(seg);
      }
      limit = SegLimit(seg);
    }
  }
  if (mode != AccessSetEMPTY) {
    AVER(base != NULL);
    AVER(limit != NULL);
    ProtSet(base, limit, mode);
  }

  shieldCacheReset(arena);
}


/* shieldCache -- consider adding a segment to the cache
 *
 * If the segment is out of sync, either sync it, or ensure it is
 * cached and the arena is suspended.
 */

static void shieldCache(Arena arena, Seg seg)
{
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  SHIELD_AVERT_CRITICAL(Seg, seg);

  if (SegIsSynced(seg) || seg->cached)
    return;

  if (SegIsExposed(seg)) {
    /* This can occur if the mutator isn't suspended, we expose a
       segment, then raise the shield on it.  In this case, the
       mutator isn't allowed to see the segment, but we don't need to
       cache it until its covered. */
    ShieldSuspend(arena);
    return;
  }

  /* Allocate shield cache if necessary. */
  /* TODO: This will try to extend the cache on every attempt, even
     if it failed last time. That might be slow. */
  if (arena->shCacheI >= arena->shCacheLength) {
    void *p;
    Res res;
    Count length;

    AVER(arena->shCacheI == arena->shCacheLength);

    if (arena->shCacheLength == 0)
      length = ShieldCacheLENGTH;
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
  if (arena->shCacheLimit >= arena->shCacheLength) {
    AVER_CRITICAL(arena->shCacheLimit == arena->shCacheLength);
    shieldFlushEntry(arena, arena->shCacheI);
  }

  arena->shCache[arena->shCacheI] = seg;
  ++arena->shCacheI;
  seg->cached = TRUE;

  if (arena->shCacheI >= arena->shCacheLimit)
    arena->shCacheLimit = arena->shCacheI;
}


/* ShieldRaise -- declare segment should be protected from mutator
 *
 * Does not immediately protect the segment, unless the segment is
 * covered and the shield cache is unavailable.
 */

void (ShieldRaise)(Arena arena, Seg seg, AccessSet mode)
{
  SHIELD_AVERT(Arena, arena);
  SHIELD_AVERT(Seg, seg);

  AVERT(AccessSet, mode);
  AVER((SegSM(seg) & mode) == AccessSetEMPTY);
  
  SegSetSM(seg, SegSM(seg) | mode); /* .inv.prot.shield preserved */

  /* ensure .inv.unsynced.suspended and .inv.unsynced.depth */
  shieldCache(arena, seg);

  /* Check cache and segment consistency. */
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


void (ShieldLower)(Arena arena, Seg seg, AccessSet mode)
{
  AVERT(Arena, arena);
  SHIELD_AVERT(Seg, seg);
  AVERT(AccessSet, mode);
  AVER(BS_INTER(SegSM(seg), mode) == mode);
  
  /* synced(seg) is not changed by the following preserving
     inv.unsynced.suspended Also inv.prot.shield preserved */
  SegSetSM(seg, BS_DIFF(SegSM(seg), mode));
  shieldProtLower(arena, seg, mode);

  /* Check cache and segment consistency. */
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


void (ShieldEnter)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(!arena->insideShield);
  AVER(arena->shDepth == 0);
  AVER(!arena->suspended);

  shieldCacheReset(arena);

  arena->insideShield = TRUE;
}


#if defined(SHIELD_DEBUG)
static void shieldDebugCheck(Arena arena)
{
  Seg seg;
  Count cached = 0;

  AVERT(Arena, arena);
  AVER(arena->insideShield || arena->shCacheLimit == 0);

  if (SegFirst(&seg, arena))
    do {
      if (arena->shCacheLimit == 0) {
        AVER(!seg->cached);
        AVER(SegIsSynced(seg));
        /* You can directly set protections here to see if it makes a
           difference. */
        /* ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg)); */
      } else {
        if (seg->cached)
          ++cached;
      }
    } while(SegNext(&seg, arena, seg));

  AVER(cached == arena->shCacheLimit);
}
#endif


/* ShieldFlush -- empty the shield cache
 *
 * .shield.flush: Flush empties the shield cache.  This needs to be
 * called before segments are destroyed as there may be references to
 * them in the cache.
 *
 * The memory for the segment may become spare, and not released back
 * to the operating system. Since we keep track of protection on
 * segments and not grains we have no way of keeping track of the
 * protection state of spare grains. We therefore flush the protection
 * to get it back into the default state (unprotected).
 */

void (ShieldFlush)(Arena arena)
{
#ifdef SHIELD_DEBUG
  shieldDebugCheck(arena);
#endif
  shieldFlushEntries(arena);
  /* Cache is empty so .inv.outside.depth holds */
  AVER(arena->shDepth == 0);
#ifdef SHIELD_DEBUG
  shieldDebugCheck(arena);
#endif
}


void (ShieldLeave)(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->insideShield);

  ShieldFlush(arena);

  /* Ensuring the mutator is running at this point guarantees
     .inv.outside.running */
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
  AVER_CRITICAL(SegDepth(seg) > 0); /* overflow */
  ++arena->shDepth;
  AVER_CRITICAL(arena->shDepth > 0); /* overflow */
  
  if (BS_INTER(SegPM(seg), mode))
    ShieldSuspend(arena);

  /* Ensure design.mps.shield.inv.expose.prot. */
  shieldProtLower(arena, seg, mode);
}


/* ShieldCover -- declare MPS no longer needs access to seg */

void (ShieldCover)(Arena arena, Seg seg)
{
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(SegPM(seg) == AccessSetEMPTY);
 
  AVER_CRITICAL(SegDepth(seg) > 0);
  SegSetDepth(seg, SegDepth(seg) - 1);
  AVER_CRITICAL(arena->shDepth > 0);
  --arena->shDepth;

  /* Ensure design.mps.shield.inv.unsynced.depth. */
  shieldCache(arena, seg);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

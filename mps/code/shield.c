/* shield.c: SHIELD IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * See: idea.shield, design.mps.shield.
 *
 * IMPORTANT: HERE BE DRAGONS! This code is subtle and
 * critical. Ensure you have read and understood design.mps.shield
 * before you touch it.
 */

#include "mpm.h"

SRCID(shield, "$Id$");


void ShieldInit(Shield shield)
{
  shield->inside = FALSE;
  shield->queue = NULL;
  shield->length = 0;
  shield->next = 0;
  shield->limit = 0;
  shield->depth = 0;
  shield->unsynced = 0;
  shield->holds = 0;
  shield->suspended = FALSE;
  shield->sig = ShieldSig;
}


void ShieldDestroyQueue(Shield shield, Arena arena)
{
  AVER(shield->limit == 0); /* queue must be empty */
  
  if (shield->length != 0) {
    AVER(shield->queue != NULL);
    ControlFree(arena, shield->queue,
                shield->length * sizeof shield->queue[0]);
    shield->queue = NULL;
    shield->length = 0;
  }
}


void ShieldFinish(Shield shield)
{
  AVER(shield->depth == 0);
  AVER(shield->unsynced == 0);
  AVER(shield->holds == 0);
  shield->sig = SigInvalid;
}


static Bool SegIsSynced(Seg seg);

Bool ShieldCheck(Shield shield)
{
  CHECKS(Shield, shield);
  CHECKL(BoolCheck(shield->inside));
  CHECKL(shield->queue == NULL || shield->length > 0);
  CHECKL(shield->limit <= shield->length);
  CHECKL(shield->next <= shield->limit);
  CHECKL(BoolCheck(shield->suspended));

  /* The mutator is not suspended while outside the shield
     (design.mps.shield.inv.outside.running). */
  CHECKL(shield->inside || !shield->suspended);

  /* If any segment is not synced, the mutator is suspended
     (design.mps.shield.inv.unsynced.suspended). */
  CHECKL(shield->unsynced == 0 || shield->suspended);

  /* If any segment is exposed, the mutator is suspended. */
  CHECKL(shield->depth == 0 || shield->suspended);

  /* The total depth is zero while outside the shield
     (design.mps.shield.inv.outside.depth). */
  CHECKL(shield->inside || shield->depth == 0);

  /* There are no unsynced segments when we're outside the shield. */
  CHECKL(shield->inside || shield->unsynced == 0);

  /* Every unsynced segment should be on the queue, because we have to
     remember to sync it before we return to the mutator. */
  CHECKL(shield->limit >= shield->unsynced);

  /* The mutator is suspeneded if there are any holds. */
  CHECKL(shield->holds == 0 || shield->suspended);

  /* This is too expensive to check all the time since we have an
     expanding shield queue that often has 16K elements instead of
     16. */
#if defined(AVER_AND_CHECK_ALL)
  {
    Count depth = 0;
    Count unsynced = 0;
    Index i;
    for (i = 0; i < shield->limit; ++i) {
      Seg seg = shield->queue[i];
      CHECKD(Seg, seg);
      depth += SegDepth(seg);
      if (!SegIsSynced(seg))
        ++unsynced;
    }
    CHECKL(depth == shield->depth);
    CHECKL(unsynced == shield->unsynced);
  }
#endif

  return TRUE;
}


Res ShieldDescribe(Shield shield, mps_lib_FILE *stream, Count depth)
{
  Res res;
  
  res = WriteF(stream, depth,
               "Shield $P {\n",    (WriteFP)shield,
               "  ", shield->inside ? "inside" : "outside", " shield\n",
               "  suspended $S\n", WriteFYesNo(shield->suspended),
               "  depth     $U\n", (WriteFU)shield->depth,
               "  next      $U\n", (WriteFU)shield->next,
               "  length    $U\n", (WriteFU)shield->length,
               "  unsynced  $U\n", (WriteFU)shield->unsynced,
               "  holds     $U\n", (WriteFU)shield->holds,
               "} Shield $P\n",    (WriteFP)shield,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


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


/* shieldSetSM -- set shield mode, maintaining sync count */

static void shieldSetSM(Shield shield, Seg seg, AccessSet mode)
{
  if (SegSM(seg) != mode) {
    if (SegIsSynced(seg)) {
      SegSetSM(seg, mode);
      ++shield->unsynced;
    } else {
      SegSetSM(seg, mode);
      if (SegIsSynced(seg)) {
        AVER(shield->unsynced > 0);
        --shield->unsynced;
      }
    }
  }
}


/* shieldSetPM -- set protection mode, maintaining sync count */

static void shieldSetPM(Shield shield, Seg seg, AccessSet mode)
{
  if (SegPM(seg) != mode) {
    if (SegIsSynced(seg)) {
      SegSetPM(seg, mode);
      ++shield->unsynced;
    } else {
      SegSetPM(seg, mode);
      if (SegIsSynced(seg)) {
        AVER(shield->unsynced > 0);
        --shield->unsynced;
      }
    }
  }
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

static void shieldSync(Shield shield, Seg seg)
{
  SHIELD_AVERT_CRITICAL(Seg, seg);

  if (!SegIsSynced(seg)) {
    ProtSet(SegBase(seg), SegLimit(seg), SegSM(seg));
    shieldSetPM(shield, seg, SegSM(seg));
  }
}


/* shieldSUspend -- suspend the mutator
 *
 * Called from inside impl.c.shield when any segment is not synced, in
 * order to provide exclusive access to the segment by the MPS.  See
 * .inv.unsynced.suspended.
 */

static void shieldSuspend(Arena arena)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
  AVER(shield->inside);

  if (!shield->suspended) {
    ThreadRingSuspend(ArenaThreadRing(arena), ArenaDeadRing(arena));
    shield->suspended = TRUE;
  }
}


/* ShieldHold -- suspend mutator access to the unprotectable
 *
 * From outside impl.c.shield, this is used when we really need to
 * lock everything against the mutator -- for example, during flip
 * when we must scan all thread registers at once.
 */

void (ShieldHold)(Arena arena)
{
  AVERT(Arena, arena);
  shieldSuspend(arena);
  ++ArenaShield(arena)->holds;
}


/* ShieldRelease -- declare mutator could be resumed
 *
 * In practice, we don't resume the mutator until ShieldLeave, but
 * this marks the earliest point at which we could resume.
 */

void (ShieldRelease)(Arena arena)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
  AVER(shield->inside);
  AVER(shield->suspended);

  AVER(shield->holds > 0);
  --shield->holds;

  /* It is only correct to actually resume the mutator here if
     shield->depth is 0, shield->unsycned is 0, and the queue is
     empty. */
  /* TODO: Consider actually doing that. */
}


/* shieldProtLower -- reduce protection on a segment
 *
 * This ensures actual prot mode does not include mode.
 */

static void shieldProtLower(Shield shield, Seg seg, AccessSet mode)
{
  /* <design/trace/#fix.noaver> */
  SHIELD_AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(AccessSet, mode);

  if (BS_INTER(SegPM(seg), mode) != AccessSetEMPTY) {
    shieldSetPM(shield, seg, BS_DIFF(SegPM(seg), mode));
    ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg));
  }
}


/* shieldDequeue -- remove a segment from the shield queue */

static Seg shieldDequeue(Shield shield, Index i)
{
  Seg seg;
  AVER(i < shield->limit);
  seg = shield->queue[i];
  AVERT(Seg, seg);
  AVER(seg->queued);
  shield->queue[i] = NULL; /* to ensure it can't get re-used */
  seg->queued = FALSE;
  return seg;
}


/* shieldFlushEntry -- flush a single entry from the queue
 *
 * If the segment is exposed we can simply dequeue it, because later
 * there will be a call to ShieldCover that will put it back on the
 * queue.  If the segment is not exposed, we can sync its protection.
 * (And if it does not have the shield raised any more, that will do
 * nothing.)
 */

static void shieldFlushEntry(Shield shield, Index i)
{
  Seg seg = shieldDequeue(shield, i);

  if (!SegIsExposed(seg))
    shieldSync(shield, seg);
}


/* shieldQueueReset -- reset shield queue pointers */

static void shieldQueueReset(Shield shield)
{
  AVER(shield->depth == 0); /* overkill: implies no segs are queued */
  AVER(shield->unsynced == 0);
  shield->next = 0;
  shield->limit = 0;
}


/* shieldQueueEntryCompare -- comparison for queue sorting */

static Compare shieldAddrCompare(Addr left, Addr right)
{
  if (left < right)
    return CompareLESS;
  else if (left == right)
    return CompareEQUAL;
  else
    return CompareGREATER;
}

static Compare shieldQueueEntryCompare(void *left, void *right, void *closure)
{
  Seg segA = left, segB = right;

  /* These checks are not critical in a hot build, but slow down cool
     builds quite a bit, so just check the signatures. */
  AVER(TESTT(Seg, segA));
  AVER(TESTT(Seg, segB));
  UNUSED(closure);

  return shieldAddrCompare(SegBase(segA), SegBase(segB));
}


/* shieldFlushEntries -- flush queue coalescing protects
 *
 * Sort the shield queue into address order, then iterate over it
 * coalescing protection work, in order to reduce the number of system
 * calls to a minimum.  This is very important on OS X, where
 * protection calls are extremely inefficient, but has no net gain on
 * Windows.
 *
 * TODO: Could we keep extending the outstanding area over memory
 * that's *not* in the queue but has the same protection mode?  Might
 * require design.mps.shield.improve.noseg.
 */

static void shieldFlushEntries(Shield shield)
{
  Addr base = NULL, limit = NULL;
  AccessSet mode;
  Index i;

  if (shield->length == 0) {
    AVER(shield->queue == NULL);
    return;
  }

  QuickSort((void *)shield->queue, shield->limit,
            shieldQueueEntryCompare, UNUSED_POINTER);

  mode = AccessSetEMPTY;
  for (i = 0; i < shield->limit; ++i) {
    Seg seg = shieldDequeue(shield, i);
    if (!SegIsSynced(seg)) {
      AVER(SegSM(seg) != AccessSetEMPTY); /* can't match first iter */
      shieldSetPM(shield, seg, SegSM(seg));
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

  shieldQueueReset(shield);
}


/* shieldQueue -- consider adding a segment to the queue
 *
 * If the segment is out of sync, either sync it, or ensure it is
 * queued and the arena is suspended.
 */

static void shieldQueue(Arena arena, Seg seg)
{
  Shield shield;
  
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  shield = ArenaShield(arena);
  SHIELD_AVERT_CRITICAL(Seg, seg);

  if (SegIsSynced(seg) || seg->queued)
    return;

  if (SegIsExposed(seg)) {
    /* This can occur if the mutator isn't suspended, we expose a
       segment, then raise the shield on it.  In this case, the
       mutator isn't allowed to see the segment, but we don't need to
       queue it until its covered. */
    shieldSuspend(arena);
    return;
  }

  /* Allocate shield queue if necessary. */
  /* TODO: This will try to extend the queue on every attempt, even
     if it failed last time. That might be slow. */
  if (shield->next >= shield->length) {
    void *p;
    Res res;
    Count length;

    AVER(shield->next == shield->length);

    if (shield->length == 0)
      length = ShieldQueueLENGTH;
    else
      length = shield->length * 2;
    
    res = ControlAlloc(&p, arena, length * sizeof shield->queue[0]);
    if (res != ResOK) {
      AVER(ResIsAllocFailure(res));
      /* Carry on with the existing queue. */
    } else {
      if (shield->length > 0) {
        Size oldSize = shield->length * sizeof shield->queue[0];
        AVER(shield->queue != NULL);
        mps_lib_memcpy(p, shield->queue, oldSize);
        ControlFree(arena, shield->queue, oldSize);
      }
      shield->queue = p;
      shield->length = length;
    }
  }

  /* Queue unavailable, so synchronize now.  Or if the mutator is not
     yet suspended and the code raises the shield on a covered
     segment, protect it now, because that's probably better than
     suspending the mutator. */
  if (shield->length == 0 || !shield->suspended) {
    shieldSync(shield, seg);
    return;
  }

  AVER_CRITICAL(shield->limit <= shield->length);
  AVER_CRITICAL(shield->next <= shield->limit);

  if (shield->next >= shield->length)
    shield->next = 0;
  AVER_CRITICAL(shield->next < shield->length);

  AVER_CRITICAL(shield->length > 0);

  /* If the limit is less than the length, then the queue array has
     yet to be filled, and next is an uninitialized entry.
     Otherwise it's the tail end from last time around, and needs to
     be flushed. */
  if (shield->limit >= shield->length) {
    AVER_CRITICAL(shield->limit == shield->length);
    shieldFlushEntry(shield, shield->next);
  }

  shield->queue[shield->next] = seg;
  ++shield->next;
  seg->queued = TRUE;

  if (shield->next >= shield->limit)
    shield->limit = shield->next;
}


/* ShieldRaise -- declare segment should be protected from mutator
 *
 * Does not immediately protect the segment, unless the segment is
 * covered and the shield queue is unavailable.
 */

void (ShieldRaise)(Arena arena, Seg seg, AccessSet mode)
{
  SHIELD_AVERT(Arena, arena);
  SHIELD_AVERT(Seg, seg);
  AVERT(AccessSet, mode);

  /* ShieldRaise does not nest. */
  AVER(BS_INTER(SegSM(seg), mode) == AccessSetEMPTY);

  /* design.mps.shield.inv.prot.shield preserved */
  shieldSetSM(ArenaShield(arena), seg, BS_UNION(SegSM(seg), mode));
  
  /* Ensure design.mps.shield.inv.unsynced.suspended and
     design.mps.shield.inv.unsynced.depth */
  shieldQueue(arena, seg);

  /* Check queue and segment consistency. */
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


/* ShieldLower -- declare segment may be accessed by mutator */

void (ShieldLower)(Arena arena, Seg seg, AccessSet mode)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
  SHIELD_AVERT(Seg, seg);
  AVERT(AccessSet, mode);
  AVER(BS_INTER(SegSM(seg), mode) == mode);

  /* SegIsSynced(seg) is not changed by the following preserving
     design.mps.shield.inv.unsynced.suspended and
     design.mps.shield.inv.prot.shield. */
  shieldSetSM(shield, seg, BS_DIFF(SegSM(seg), mode));
  /* TODO: Do we need to promptly call shieldProtLower here?  It
     loses the opportunity to coalesce the protection call. It would
     violate design.mps.shield.prop.inside.access. */
  shieldProtLower(shield, seg, mode);

  /* Check queue and segment consistency. */
  AVERT(Arena, arena);
  AVERT(Seg, seg);
}


/* ShieldEnter -- enter the shield, allowing exposes */

void (ShieldEnter)(Arena arena)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
  AVER(!shield->inside);
  AVER(shield->depth == 0);
  AVER(!shield->suspended);

  shieldQueueReset(shield);

  shield->inside = TRUE;
}


/* shieldDebugCheck -- expensive consistency check
 *
 * While developing the shield it is very easy to make a consistency
 * mistake that causes random corruption of the heap, usually because
 * all the attempts to avoid protection and suspension end up failing
 * to enforce design.mps.shield.prop.mutator.access.  In these cases,
 * try enabling SHIELD_DEBUG and extending this code as necessary.
 *
 * The basic idea is to iterate over *all* segments and check
 * consistency with the arena and shield queue.
 */

#if defined(SHIELD_DEBUG)
static void shieldDebugCheck(Arena arena)
{
  Shield shield;
  Seg seg;
  Count queued = 0;

  AVERT(Arena, arena);
  shield = ShieldArena(arena);
  AVER(shield->inside || shield->limit == 0);

  if (SegFirst(&seg, arena))
    do {
      if (shield->limit == 0) {
        AVER(!seg->queued);
        AVER(SegIsSynced(seg));
        /* You can directly set protections here to see if it makes a
           difference. */
        /* ProtSet(SegBase(seg), SegLimit(seg), SegPM(seg)); */
      } else {
        if (seg->queued)
          ++queued;
      }
    } while(SegNext(&seg, arena, seg));

  AVER(queued == shield->limit);
}
#endif


/* ShieldFlush -- empty the shield queue
 *
 * .shield.flush: Flush empties the shield queue.  This needs to be
 * called before segments in the queue are destroyed, as there may be
 * references to them in the queue.
 *
 * The memory for the segment may become spare, and not released back
 * to the operating system. Since we keep track of protection on
 * segments and not grains we have no way of keeping track of the
 * protection state of spare grains. We therefore flush the protection
 * to get it back into the default state (unprotected).  See also
 * design.mps.shield.improv.noseg.
 */

void (ShieldFlush)(Arena arena)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
#ifdef SHIELD_DEBUG
  shieldDebugCheck(arena);
#endif
  shieldFlushEntries(shield);
  /* Queue is empty so .inv.outside.depth holds */
  AVER(shield->depth == 0);
#ifdef SHIELD_DEBUG
  shieldDebugCheck(arena);
#endif
}


/* ShieldLeave -- leave the shield, protect segs from mutator */

void (ShieldLeave)(Arena arena)
{
  Shield shield;
  
  AVERT(Arena, arena);
  shield = ArenaShield(arena);
  AVER(shield->inside);
  AVER(shield->depth == 0); /* no pending covers */
  AVER(shield->holds == 0);

  ShieldFlush(arena);

  AVER(shield->unsynced == 0); /* everything back in sync */

  /* Ensuring the mutator is running at this point guarantees
     .inv.outside.running */
  if (shield->suspended) {
    ThreadRingResume(ArenaThreadRing(arena), ArenaDeadRing(arena));
    shield->suspended = FALSE;
  }

  shield->inside = FALSE;
}



/* ShieldExpose -- allow the MPS access to a segment while denying the mutator
 *
 * The first expose of a shielded segment suspends the mutator to
 * ensure the MPS has exclusive access.
 */

void (ShieldExpose)(Arena arena, Seg seg)
{
  Shield shield;
  AccessSet mode = AccessREAD | AccessWRITE;

  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  shield = ArenaShield(arena);
  AVER_CRITICAL(shield->inside);

  SegSetDepth(seg, SegDepth(seg) + 1);
  AVER_CRITICAL(SegDepth(seg) > 0); /* overflow */
  ++shield->depth;
  AVER_CRITICAL(shield->depth > 0); /* overflow */
  
  if (BS_INTER(SegPM(seg), mode) != AccessSetEMPTY)
    shieldSuspend(arena);

  /* Ensure design.mps.shield.inv.expose.prot. */
  /* TODO: Mass exposure -- see
     design.mps.shield.improv.mass-expose. */
  shieldProtLower(shield, seg, mode);
}


/* ShieldCover -- declare MPS no longer needs access to seg */

void (ShieldCover)(Arena arena, Seg seg)
{
  Shield shield;
  
  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Arena, arena);
  shield = ArenaShield(arena);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(SegPM(seg) == AccessSetEMPTY);
 
  AVER_CRITICAL(SegDepth(seg) > 0);
  SegSetDepth(seg, SegDepth(seg) - 1);
  AVER_CRITICAL(shield->depth > 0);
  --shield->depth;

  /* Ensure design.mps.shield.inv.unsynced.depth. */
  shieldQueue(arena, seg);
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

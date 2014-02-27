/* zonedcbs.c: ZONED COALESCING BLOCK STRUCTURE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * A Zone CBS is like a CBS but allows for efficient allocation in zones.
 * Allocation in zones gives control over some parts of an object's address,
 * so that we can later apply fast filters on the critical path.
 * The Zone CBS is mainly used by the arena to allocate blocks to pools.
 *
 * Can be stressed by setting a small arena size.
 *
 * This code is essentially an optimisation of CBSFindFirstInZones, but
 * doesn't have quite the same effect.  FIXME: Be clearer.
 */

#include "zonedcbs.h"
#include "mpm.h"
#include "cbs.h"


#define ZonedCBSBlockPool(zcbs)   RVALUE((zcbs)->blockPool)
#define ZonedCBSFreeCBS(zcbs)     (&(zcbs)->freeStruct)
#define ZonedCBSZoneCBS(zcbs, z)  (&(zcbs)->zoneStruct[z])
#define ZonedCBSNZones(zcbs)      NELEMS((zcbs)->zoneStruct)


/* ZonedCBSCheck -- check consistency of zoned CBS structure */

Bool ZonedCBSCheck(ZonedCBS zcbs)
{
  Index i;

  CHECKS(ZonedCBS, zcbs);
  CHECKU(Arena, zcbs->arena);
  CHECKD(Pool, ZonedCBSBlockPool(zcbs));
  CHECKD(CBS, ZonedCBSFreeCBS(zcbs));
  CHECKL(ZonedCBSNZones(zcbs) == sizeof(ZoneSet) * CHAR_BIT);
  CHECKL(ZonedCBSNZones(zcbs) == NELEMS(zcbs->zoneStruct));
  for (i = 0; i < ZonedCBSNZones(zcbs); ++i)
    CHECKD(CBS, ZonedCBSZoneCBS(zcbs, i));

  /* TODO: Thorough check summing CBSs against totals.  The sum of the
     sizes of the contents of the zone CBSs and the freeCBS should equal
     the total allocatable free space in the chunks.  The CBS ADT
     probably ought to maintain totals like this too. */

  return TRUE;
}


/* ZonedCBSInit -- initialise a Zoned CBS */

Res ZonedCBSInit(ZonedCBS zcbs, Arena arena, Pool blockPool, Align alignment)
{
  Index i;
  Res res;

  AVER(zcbs != NULL);
  AVERT(Pool, blockPool);

  zcbs->arena = arena;
  zcbs->blockPool = blockPool;
  
  /* Initialise the freeCBS. */
  MPS_ARGS_BEGIN(cbsiArgs) {
    MPS_ARGS_ADD(cbsiArgs, CBSBlockPool, blockPool);
    MPS_ARGS_DONE(cbsiArgs);
    res = CBSInit(arena, ZonedCBSFreeCBS(zcbs), zcbs, alignment, TRUE, cbsiArgs);
  } MPS_ARGS_END(cbsiArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failCBSInit;
  /* Note that although freeCBS is initialised, it doesn't have any memory
     for its blocks, so hasFreeCBS remains FALSE until later. */

  /* Initialise the zoneCBSs. */
  for (i = 0; i < NELEMS(zcbs->zoneStruct); ++i) {
    MPS_ARGS_BEGIN(cbsiArgs) {
      MPS_ARGS_ADD(cbsiArgs, CBSBlockPool, blockPool);
      MPS_ARGS_DONE(cbsiArgs);
      res = CBSInit(arena, ZonedCBSZoneCBS(zcbs, i), arena, alignment, TRUE, cbsiArgs);
    } MPS_ARGS_END(cbsiArgs);
    AVER(res == ResOK); /* no allocation, no failure expected */
    if (res != ResOK)
      goto failZoneCBSInit;
  }

  zcbs->sig = ZonedCBSSig;
  AVERT(ZonedCBS, zcbs);
  
  return ResOK;

failZoneCBSInit:
  while (i > 0) {
    --i;
    CBSFinish(ZonedCBSZoneCBS(zcbs, i));
  }
  CBSFinish(ZonedCBSFreeCBS(zcbs));
failCBSInit:
  return res;
}


/* ZonedCBSFinish -- finish the zoned CBS */

void ZonedCBSFinish(ZonedCBS zcbs)
{
  Index i;
  
  AVERT(ZonedCBS, zcbs);

  zcbs->sig = SigInvalid;

  /* FIXME: Should be asserting that CBSs are empty?  Could iterate over
     chunks and remove their ranges to make sure things are consistent. */

  for (i = 0; i < ZonedCBSNZones(zcbs); ++i)
    CBSFinish(ZonedCBSZoneCBS(zcbs, i));
  CBSFinish(ZonedCBSFreeCBS(zcbs));
}


/* ZonedCBSInsert -- insert a range into the zoned CBS
 *
 * We just insert it into the free CBS.  It will get split up and cached
 * in the zoned CBSs by ZonedCBSFindFirst and ZonedCBSFindLast.
 */

Res ZonedCBSInsert(Range rangeReturn, ZonedCBS zcbs, Range range)
{
  ZoneSet zs;

  AVERT(ZonedCBS, zcbs);

  /* TODO: Consider moving empty zone stripes back to freeCBS. At the
     moment we do not do this, partly for simplicity, and partly to
     keep large objects apart from smaller ones, at the possible cost
     of address space fragmentation.  We probably do not want to do it
     eagerly in any case, but lazily if we're unable to find address
     space, even though that reduces first-fit. */

  zs = ZoneSetOfRange(zcbs->arena, RangeBase(range), RangeLimit(range));
  if (ZoneSetIsSingle(zs)) {
    Index zone = AddrZone(zcbs->arena, RangeBase(range));
    CBS zoneCBS = ZonedCBSZoneCBS(zcbs, zone);
    return CBSInsert(rangeReturn, zoneCBS, range);
  }

  return CBSInsert(rangeReturn, ZonedCBSFreeCBS(zcbs), range);
}


/* ZonedCBSDelete -- delete a range from the zoned CBS
 *
 * The range may be split between the zone CBSs and the free CBS on zone
 * stripe boundaries, in which case we have to iterate.
 *
 * .delete.exists: The range must exist in the zoned CBS.  To make the
 * zoned CBS more general we'd need to unwind the error case where we
 * found a hole, putting the range we'd already found back.  This isn't
 * required by the arena.
 *
 * FIXME: Document guarantees about res.
 */

Res ZonedCBSDelete(Range oldRange, ZonedCBS zcbs, Range range)
{
  Res res;
  ZoneSet zs;
  Addr base, limit;

  AVER(oldRange != NULL);
  AVERT(ZonedCBS, zcbs);
  AVERT(Range, range);

  zs = ZoneSetOfRange(zcbs->arena, RangeBase(range), RangeLimit(range));
  if (ZoneSetIsSingle(zs)) {
    Index zone = AddrZone(zcbs->arena, RangeBase(range));
    CBS zoneCBS = ZonedCBSZoneCBS(zcbs, zone);
    res = CBSDelete(oldRange, zoneCBS, range);
    if (res != ResFAIL) /* block was in zone CBS */
      return res;
  }

  res = CBSDelete(oldRange, ZonedCBSFreeCBS(zcbs), range);
  if (res != ResFAIL) /* block was in free CBS */
    return res;
  
  /* The range may be divided between the free CBS and zone CBSs. */
  base = RangeBase(range);
  limit = RangeLimit(range);
  while (base < limit) {
    Addr stripeLimit;
    CBS zoneCBS;
    RangeStruct stripe, oldStripe;
    Index zone;

    stripeLimit = AddrAlignUp(AddrAdd(base, 1), ArenaStripeSize(zcbs->arena));
    if (stripeLimit > limit)
      stripeLimit = limit;

    zone = AddrZone(zcbs->arena, base);
    AVER(AddrZone(zcbs->arena, AddrSub(stripeLimit, 1)) == zone);
    zoneCBS = ZonedCBSZoneCBS(zcbs, zone);

    RangeInit(&stripe, base, stripeLimit);
    res = CBSDelete(&oldStripe, ZonedCBSFreeCBS(zcbs), &stripe);

    /* Optimisation: delete and skip over the rest of the block we
       found in the free CBS, up to the next block that's in a zone CBS
       (or the end). */
    if (res == ResOK && !RangesEqual(&oldStripe, &stripe)) {
      Addr skipLimit = RangeLimit(&oldStripe);
      if (skipLimit > limit)
        skipLimit = limit;
      if (stripeLimit < skipLimit) {
        RangeStruct restOfBlock;
        RangeInit(&restOfBlock, stripeLimit, skipLimit);
        res = CBSDelete(&oldStripe, ZonedCBSFreeCBS(zcbs), &restOfBlock);
        AVER(res == ResOK); /* FIXME: is this right? */
        AVER(RangesEqual(&oldStripe, &restOfBlock));
        base = skipLimit;
        continue;
      }
    }

    if (res == ResFAIL) {
      res = CBSDelete(&oldStripe, zoneCBS, &stripe);
      AVER(res != ResFAIL); /* .delete.exists */
      AVER(RangesEqual(&oldStripe, &stripe));
    }

    AVER(res == ResOK); /* FIXME: end of range, shouldn't fail? */

    base = stripeLimit;
  }
  
  /* Shouldn't be any other kind of failure. */
  AVER(res == ResOK);
  return ResOK;
}


/* ZonedCBSFindFirst, ZonedCBSFindLast -- find a range in the zoned CBS */


static Res ZonedCBSFindInZones(Range rangeReturn,
                               Range oldRangeReturn,
                               ZonedCBS zcbs,
                               ZoneSet zones,
                               Size size,
                               FindDelete findDelete,
                               Bool high)
{
  Index i;
  CBSFindMethod find = high ? CBSFindLast : CBSFindFirst;

  /* We could check zoneCBS if size > stripeSize to avoid looking in the
     zoneCBSs, but this probably isn't a win for arena allocation. */

  /* TODO: Does "high" really make sense for zone stripes? */
  /* TODO: How do we disable zones anyway?  Just make zoneShift = WORD_WIDTH?
     DL points out that if we had two zones, they'd both be blacklisted. */

  /* Even though we have no guarantee that zone zero is at the bottom end
     of anything, it makes sense to reverse the search when "high" is set,
     because the point of it (presumably) is to separate memory usage into
     two sets (high and low) that avoid interference. */

  /* TODO: Consider masking zones against a ZoneSet of non-empty zone CBSs */

  /* TODO: ZONESET_FOR using __builtin_ffsl or similar. */

  for (i = 0; i < ZonedCBSNZones(zcbs); ++i) {
    Index zone = high ? ZonedCBSNZones(zcbs) - i - 1 : i;
    if (ZoneSetIsMember(zones, zone) &&
        find(rangeReturn, oldRangeReturn, ZonedCBSZoneCBS(zcbs, zone),
             size, findDelete))
      return ResOK;
  }

  return ResRESOURCE;
}

static Res ZonedCBSFindInFree(Range rangeReturn,
                              Range oldRangeReturn,
                              ZonedCBS zcbs,
                              ZoneSet zones,
                              Size size,
                              FindDelete findDelete,
                              Bool high)
{
  CBSFindInZonesMethod find = high ? CBSFindLastInZones : CBSFindFirstInZones;

  UNUSED(findDelete); /* FIXME: why is this so? */

  return find(rangeReturn, oldRangeReturn, ZonedCBSFreeCBS(zcbs), size,
              zcbs->arena, zones);
}

Res ZonedCBSFind(Range rangeReturn,
                 Range oldRangeReturn,
                 ZonedCBS zcbs,
                 ZoneSet zones,
                 Size size,
                 FindDelete findDelete,
                 Bool high)
{
  RangeStruct restRange;
  Addr allocLimit, stripeLimit, oldLimit, limit;
  Res res;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(ZonedCBS, zcbs);
  AVER(size > 0);
  /* ZoneSet arbitrary.  TODO: Make a ZoneSetCheck anyway. */
  AVER(BoolCheck(high));

  /* TODO: Consider the other FindDelete cases. */
  AVER(findDelete == FindDeleteLOW || findDelete == FindDeleteHIGH);
  AVER((findDelete == FindDeleteHIGH) == high);

  res = ZonedCBSFindInZones(rangeReturn, oldRangeReturn,
                            zcbs, zones, size,
                            findDelete, high);
  if (res != ResRESOURCE)
    return res;

  res = ZonedCBSFindInFree(rangeReturn, oldRangeReturn,
                           zcbs, zones, size,
                           findDelete, high);
  if (res != ResOK)
    return res;

  /* TODO: We may have failed to find because the address space is
     fragmented between the zone CBSs and the free CBS.  This isn't
     a very important case for the arena, but it does make the Zone CBS
     technically incorrect as a representation of a set of ranges.
     Flush the zone CBSs back to the free CBS? */

  /* Add the rest of the zone stripe to the zoneCBS so that subsequent
     allocations in the zone are fast.  This is what the ZonedCBS is
     all about! */

  allocLimit = RangeLimit(rangeReturn);
  stripeLimit = AddrAlignUp(allocLimit, ArenaStripeSize(zcbs->arena));
  oldLimit = RangeLimit(oldRangeReturn);
  limit = oldLimit < stripeLimit ? oldLimit : stripeLimit;
  RangeInit(&restRange, allocLimit, limit);
  AVER(RangesNest(oldRangeReturn, &restRange));

  if (allocLimit < limit) {
    Index zone;
    CBS zoneCBS;
    RangeStruct oldRange;

    res = CBSDelete(&oldRange, ZonedCBSFreeCBS(zcbs), &restRange);
    AVER(res == ResOK); /* we should just be bumping up a base */
    zone = AddrZone(zcbs->arena, RangeBase(&restRange));
    zoneCBS = ZonedCBSZoneCBS(zcbs, zone);

    res = CBSInsert(&oldRange, zoneCBS, &restRange);
    AVER(res != ResOK || RangesEqual(&oldRange, &restRange)); /* shouldn't coalesce */
    if (res != ResOK) { /* disasterously short on memory, so put it back */
      res = CBSInsert(&oldRange, ZonedCBSFreeCBS(zcbs), &restRange);
      AVER(res == ResOK); /* should just be lowering a base */
    }
  }

  return ResOK;
}

/*
Res ZonedCBSFindFirst(Range rangeReturn, Range oldRangeReturn,
                      ZonedCBS zcbs, ZoneSet zones,
                      Size size, FindDelete findDelete)
{
  return ZonedCBSFind(rangeReturn, oldRangeReturn, zcbs, zones, size,
                      findDelete, FALSE);
}

Bool ZonedCBSFindLast(Range rangeReturn, Range oldRangeReturn,
                      ZonedCBS zcbs, ZoneSet zones,
                      Size size, FindDelete findDelete)
{
  return ZonedCBSFind(rangeReturn, oldRangeReturn, zcbs, zones, size,
                      findDelete, TRUE);
}
*/


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

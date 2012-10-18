/* seg.c: SEGMENTS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .design: The design for this module is <design/seg/>.
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the generic segment interface.
 * It defines the interface functions and two useful segment classes:
 * .purpose.class.seg: Class Seg is a class which is as simple
 * as efficiency demands permit.  (It includes fields for storing colour
 * for efficiency).  It may be subclassed by clients of the module.
 * .purpose.class.seg-gc: Class GCSeg is a concrete class support all
 * all current GC features, and providing full backwards compatibility
 * with "old-style" segments.  It may be subclassed by clients of the
 * module.
 *
 * TRANSGRESSIONS
 *
 * .check.shield: The "pm", "sm", and "depth" fields are not checked by
 * SegCheck, because I haven't spent time working out the invariants.
 * We should certainly work them out, by studying <code/shield.c>, and
 * assert things about shielding, protection, shield cache consistency,
 * etc.  richard 1997-04-03
 */

#include "tract.h"
#include "mpm.h"

SRCID(seg, "$Id$");


/* SegGCSeg -- convert generic Seg to GCSeg */

#define SegGCSeg(seg)             ((GCSeg)(seg))

/* SegPoolRing -- Pool ring accessor */

#define SegPoolRing(seg)          (&(seg)->poolRing)


/* forward declarations */

static void SegFinish(Seg seg);

static Res SegInit(Seg seg, Pool pool, Addr base, Size size,
                   Bool withReservoirPermit, va_list args);


/* Generic interface support */


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegClass class, SegPref pref,
             Size size, Pool pool, Bool withReservoirPermit, ...)
{
  Res res;
  Arena arena;
  Seg seg;
  Addr base;
  va_list args;
  void *p;

  AVER(segReturn != NULL);
  AVERT(SegClass, class);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(BoolCheck(withReservoirPermit));

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));

  /* allocate the memory from the arena */
  res = ArenaAlloc(&base, pref, size, pool, withReservoirPermit);
  if (res != ResOK)
    goto failArena;

  /* allocate the segment object from the control pool */
  res = ControlAlloc(&p, arena, class->size, withReservoirPermit);
  if (res != ResOK)
    goto failControl;
  seg = p;

  va_start(args, withReservoirPermit);
  seg->class = class;
  res = SegInit(seg, pool, base, size, withReservoirPermit, args);
  va_end(args);
  if (res != ResOK)
    goto failInit;

  EVENT5(SegAlloc, arena, seg, SegBase(seg), size, pool);
  *segReturn = seg;
  return ResOK;

failInit:
  ControlFree(arena, seg, class->size);
failControl:
  ArenaFree(base, size, pool);
failArena:
  EVENT3(SegAllocFail, arena, size, pool);
  return res;
}


/* SegFree -- free a segment to the arena */

void SegFree(Seg seg)
{
  Arena arena;
  Pool pool;
  Addr base;
  Size size;
  SegClass class;

  AVERT(Seg, seg);
  pool = SegPool(seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  base = SegBase(seg);
  size = SegSize(seg);
  class = seg->class;

  SegFinish(seg);
  ControlFree(arena, seg, class->size);
  ArenaFree(base, size, pool);

  EVENT2(SegFree, arena, seg);
  return;
}


/* SegInit -- initialize a segment */

static Res SegInit(Seg seg, Pool pool, Addr base, Size size,
                   Bool withReservoirPermit, va_list args)
{
  Tract tract;
  Addr addr, limit;
  Size align;
  Arena arena;
  SegClass class;
  Res res;

  AVER(seg != NULL);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);
  AVER(AddrIsAligned(base, align));
  AVER(SizeIsAligned(size, align));
  class = seg->class;
  AVERT(SegClass, class);
  AVER(BoolCheck(withReservoirPermit));

  limit = AddrAdd(base, size);
  seg->limit = limit;
  seg->rankSet = RankSetEMPTY;
  seg->white = TraceSetEMPTY;
  seg->nailed = TraceSetEMPTY;
  seg->grey = TraceSetEMPTY;
  seg->pm = AccessSetEMPTY;
  seg->sm = AccessSetEMPTY;
  seg->depth = 0;
  seg->firstTract = NULL;

  seg->sig = SegSig;  /* set sig now so tract checks will see it */

  TRACT_FOR(tract, addr, arena, base, limit) {
    AVER(TractCheck(tract));  /* <design/check/#type.no-sig> */
    AVER(TractP(tract) == NULL);
    AVER(!TractHasSeg(tract));
    AVER(TractPool(tract) == pool);
    AVER(TractWhite(tract) == TraceSetEMPTY);
    TRACT_SET_SEG(tract, seg);
    if (addr == base) {
      AVER(seg->firstTract == NULL);
      seg->firstTract = tract;
    }
    AVER(seg->firstTract != NULL);
  }
  AVER(addr == seg->limit);

  RingInit(SegPoolRing(seg));

  /* Class specific initialization comes last */
  res = class->init(seg, pool, base, size, withReservoirPermit, args);
  if (res != ResOK)
    goto failInit;
   
  AVERT(Seg, seg);
  RingAppend(&pool->segRing, SegPoolRing(seg));
  return ResOK;

failInit:
  RingFinish(SegPoolRing(seg));
  TRACT_FOR(tract, addr, arena, base, limit) {
    AVER(TractCheck(tract));  /* <design/check/#type.no-sig> */
    TRACT_UNSET_SEG(tract);
  }
  seg->sig = SigInvalid;
  return res;
}


/* SegFinish -- finish a segment */

static void SegFinish(Seg seg)
{
  Arena arena;
  Addr addr, limit;
  Tract tract;
  SegClass class;

  AVERT(Seg, seg);
  class = seg->class;
  AVERT(SegClass, class);

  arena = PoolArena(SegPool(seg));
  if (seg->sm != AccessSetEMPTY) {
    ShieldLower(arena, seg, seg->sm);
  }

  /* Class specific finishing cames first */
  class->finish(seg);

  seg->rankSet = RankSetEMPTY;

  /* See <code/shield.c#shield.flush> */
  ShieldFlush(PoolArena(SegPool(seg)));

  limit = SegLimit(seg);
  
  TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, limit) {
    AVER(TractCheck(tract));  /* <design/check/#type.no-sig> */
    TractSetWhite(tract, TraceSetEMPTY);
    TRACT_UNSET_SEG(tract);
  }
  AVER(addr == seg->limit);

  RingRemove(SegPoolRing(seg));
  RingFinish(SegPoolRing(seg));

  seg->sig = SigInvalid;

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see <code/shield.c#def.depth>). */
  AVER(seg->depth == 0);
  /* Check not shielded or protected (so that pages in hysteresis */
  /* fund are not protected) */
  AVER(seg->sm == AccessSetEMPTY);
  AVER(seg->pm == AccessSetEMPTY);
 
}


/* SegSetGrey -- change the greyness of a segment
 *
 * Sets the segment greyness to the trace set grey.
 */

void SegSetGrey(Seg seg, TraceSet grey)
{
  AVERT(Seg, seg);
  AVER(TraceSetCheck(grey));
  AVER(SegRankSet(seg) != RankSetEMPTY);

  /* Don't dispatch to the class method if there's no actual change in
     greyness, or if the segment doesn't contain any references. */
  if (grey != SegGrey(seg) && SegRankSet(seg) != RankSetEMPTY)
    seg->class->setGrey(seg, grey);
}


/* SegSetWhite -- change the whiteness of a segment
 *
 * Sets the segment whiteness to the trace set ts.
 */

void SegSetWhite(Seg seg, TraceSet white)
{
  AVERT(Seg, seg);
  AVER(TraceSetCheck(white));
  seg->class->setWhite(seg, white);
}


/* SegSetRankSet -- set the rank set of a segment
 *
 * The caller must set the summary to empty before setting the rank
 * set to empty.  The caller must set the rank set to non-empty before
 * setting the summary to non-empty.
 */

void SegSetRankSet(Seg seg, RankSet rankSet)
{
  AVERT(Seg, seg);
  AVER(RankSetCheck(rankSet));
  AVER(rankSet != RankSetEMPTY || SegSummary(seg) == RefSetEMPTY);
  seg->class->setRankSet(seg, rankSet);
}


/* SegSetSummary -- change the summary on a segment */

void SegSetSummary(Seg seg, RefSet summary)
{
  AVERT(Seg, seg);
  AVER(summary == RefSetEMPTY || SegRankSet(seg) != RankSetEMPTY);

#ifdef PROTECTION_NONE
  summary = RefSetUNIV;
#endif
  if (summary != SegSummary(seg))
    seg->class->setSummary(seg, summary);
}


/* SegSetRankAndSummary -- set both the rank set and the summary */

void SegSetRankAndSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  AVERT(Seg, seg); 
  AVER(RankSetCheck(rankSet));

#ifdef PROTECTION_NONE
  if (rankSet != RankSetEMPTY) {
    summary = RefSetUNIV;
  }
#endif
  seg->class->setRankSummary(seg, rankSet, summary);
}


/* SegBuffer -- return the buffer of a segment */

Buffer SegBuffer(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);  /* .seg.critical */
  return seg->class->buffer(seg);
}


/* SegSetBuffer -- change the buffer on a segment */

void SegSetBuffer(Seg seg, Buffer buffer)
{
  AVERT(Seg, seg);
  if (buffer != NULL)
    AVERT(Buffer, buffer);
  seg->class->setBuffer(seg, buffer);
}


/* SegDescribe -- describe a segment */

Res SegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  Pool pool;

  if (!TESTT(Seg, seg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  pool = SegPool(seg);

  res = WriteF(stream,
               "Segment $P [$A,$A) {\n", (WriteFP)seg,
               (WriteFA)SegBase(seg), (WriteFA)SegLimit(seg),
               "  class $P (\"$S\")\n",
               (WriteFP)seg->class, seg->class->name,
               "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               NULL);
  if (res != ResOK) return res;

  res = seg->class->describe(seg, stream);
  if (res != ResOK) return res;

  res = WriteF(stream, "\n",
               "} Segment $P\n", (WriteFP)seg, NULL);
  return res;
}


/* .seg.critical: These seg functions are low-level and used
 * through-out. They are therefore on the
 * [critical path](../design/critical-path.txt) and their AVERs are so-marked.
 */

/* SegBase -- return the base address of a seg */

Addr (SegBase)(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return SegBase(seg);
}


/* SegLimit -- return the limit address of a segment */

Addr (SegLimit)(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return SegLimit(seg);
}


/* SegSize -- return the size of a seg */

Size SegSize(Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return AddrOffset(SegBase(seg), SegLimit(seg));
}


/* SegOfAddr -- return the seg the given address is in, if any */

Bool SegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  Tract tract;
  AVER_CRITICAL(segReturn != NULL);   /* .seg.critical */
  AVERT_CRITICAL(Arena, arena);       /* .seg.critical */
  if (TractOfAddr(&tract, arena, addr)) {
    return TRACT_SEG(segReturn, tract);
  } else {
    return FALSE;
  }
}


/* SegFirst -- return the first seg in the arena
 *
 * This is used to start an iteration over all segs in the arena.
 */

Bool SegFirst(Seg *segReturn, Arena arena)
{
  Tract tract;
  AVER(segReturn != NULL);
  AVERT(Arena, arena);

  if (TractFirst(&tract, arena)) {
    do {
      Seg seg;
      if (TRACT_SEG(&seg, tract)) {
        *segReturn = seg;
        return TRUE;
      }
    } while (TractNext(&tract, arena, TractBase(tract)));
  }
  return FALSE;
}


/* SegNext -- return the "next" seg in the arena
 *
 * This is used as the iteration step when iterating over all
 * segs in the arena.
 *
 * SegNext finds the seg with the lowest base address which is
 * greater than a specified address.  The address must be (or once
 * have been) the base address of a seg.
 */

Bool SegNext(Seg *segReturn, Arena arena, Addr addr)
{
  Tract tract;
  Addr base = addr;
  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  AVERT_CRITICAL(Arena, arena);

  while (TractNext(&tract, arena, base)) {
    Seg seg;
    if (TRACT_SEG(&seg, tract)) {
      if (tract == seg->firstTract) {
        *segReturn = seg;
        return TRUE;
      } else {
        /* found the next tract in a large segment */
        /* base & addr must be the base of this segment */
        AVER_CRITICAL(TractBase(seg->firstTract) == addr);
        AVER_CRITICAL(addr == base);
        /* set base to the last tract in the segment */
        base = AddrSub(seg->limit, ArenaAlign(arena));
        AVER_CRITICAL(base > addr);
      }
    } else {
      base = TractBase(tract);
    }
  }
  return FALSE;
}



/* SegMerge -- Merge two adjacent segments
 *
 * See <design/seg/#merge>
 */

Res SegMerge(Seg *mergedSegReturn, Seg segLo, Seg segHi,
             Bool withReservoirPermit, ...)
{
  SegClass class;
  Addr base, mid, limit;
  Arena arena;
  Res res;
  va_list args;

  AVER(NULL != mergedSegReturn);
  AVERT(Seg, segLo);
  AVERT(Seg, segHi);
  class = segLo->class;
  AVER(segHi->class == class);
  AVER(SegPool(segLo) == SegPool(segHi));
  base = SegBase(segLo);
  mid = SegLimit(segLo);
  limit = SegLimit(segHi);
  AVER(SegBase(segHi) == SegLimit(segLo));
  AVER(BoolCheck(withReservoirPermit));
  arena = PoolArena(SegPool(segLo));

  ShieldFlush(arena);  /* see <design/seg/#split-merge.shield> */

  /* Invoke class-specific methods to do the merge */
  va_start(args, withReservoirPermit);
  res = class->merge(segLo, segHi, base, mid, limit,
                     withReservoirPermit, args);
  va_end(args);
  if (ResOK != res)
    goto failMerge;

  EVENT3(SegMerge, segLo, segHi, withReservoirPermit);
  /* Deallocate segHi object */
  ControlFree(arena, segHi, class->size);
  AVERT(Seg, segLo);
  *mergedSegReturn = segLo;
  return ResOK;

failMerge:
  AVERT(Seg, segLo); /* check original segs are still valid */
  AVERT(Seg, segHi);
  return res;
}


/* SegSplit -- Split a segment
 *
 * The segment is split at the indicated position.
 * See <design/seg/#split>
 */

Res SegSplit(Seg *segLoReturn, Seg *segHiReturn, Seg seg, Addr at,
             Bool withReservoirPermit, ...)
{
  Addr base, limit;
  SegClass class;
  Seg segNew;
  Arena arena;
  Res res;
  va_list args;
  void *p;

  AVER(NULL != segLoReturn);
  AVER(NULL != segHiReturn);
  AVERT(Seg, seg);
  class = seg->class;
  arena = PoolArena(SegPool(seg));
  base = SegBase(seg);
  limit = SegLimit(seg);
  AVERT(Arena, arena);
  AVER(AddrIsAligned(at, arena->alignment));
  AVER(at > base);
  AVER(at < limit);
  AVER(BoolCheck(withReservoirPermit));

  ShieldFlush(arena);  /* see <design/seg/#split-merge.shield> */

  /* Allocate the new segment object from the control pool */
  res = ControlAlloc(&p, arena, class->size, withReservoirPermit);
  if (ResOK != res)
    goto failControl;
  segNew = p;

  /* Invoke class-specific methods to do the split */
  va_start(args, withReservoirPermit);
  res = class->split(seg, segNew, base, at, limit,
                     withReservoirPermit, args);
  va_end(args);
  if (ResOK != res)
    goto failSplit;

  EVENT4(SegSplit, seg, segNew, seg, at);
  AVERT(Seg, seg);
  AVERT(Seg, segNew);
  *segLoReturn = seg;
  *segHiReturn = segNew;
  return ResOK;

failSplit:
  ControlFree(arena, segNew, class->size);
failControl:
  AVERT(Seg, seg); /* check the original seg is still valid */
  return res;
}


/* Class Seg -- The most basic segment class
 *
 * .seg.method.check: Many seg methods are lightweight and used
 * frequently. Their parameters are checked by the corresponding
 * dispatching function, and so the their parameter AVERs are
 * marked as critical.
 */


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  Tract tract;
  Arena arena;
  Pool pool;
  Addr addr;
  Size align;
 
  CHECKS(Seg, seg);
  CHECKL(TraceSetCheck(seg->white));

  /* can't assume nailed is subset of white - mightn't be during whiten */
  /* CHECKL(TraceSetSub(seg->nailed, seg->white)); */
  CHECKL(TraceSetCheck(seg->grey));
  CHECKL(TractCheck(seg->firstTract));  /* <design/check/#type.no-sig> */
  pool = SegPool(seg);
  CHECKU(Pool, pool);
  arena = PoolArena(pool);
  CHECKU(Arena, arena);
  align = ArenaAlign(arena);
  CHECKL(AddrIsAligned(TractBase(seg->firstTract), align));
  CHECKL(AddrIsAligned(seg->limit, align));
  CHECKL(seg->limit > TractBase(seg->firstTract));

  /* Each tract of the segment must agree about white traces */
  TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, seg->limit) {
    Seg trseg = NULL; /* suppress compiler warning */

    CHECKL(TractCheck(tract));  /* <design/check/#type.no-sig> */
    CHECKL(TRACT_SEG(&trseg, tract) && (trseg == seg));
    CHECKL(TractWhite(tract) == seg->white);
    CHECKL(TractPool(tract) == pool);
  }
  CHECKL(addr == seg->limit);

  /* The segment must belong to some pool, so it should be on a */
  /* pool's segment ring.  (Actually, this isn't true just after */
  /* the segment is initialized.) */
  /*  CHECKL(RingNext(&seg->poolRing) != &seg->poolRing); */

  CHECKL(RingCheck(&seg->poolRing));
   
  /* "pm", "sm", and "depth" not checked.  See .check.shield. */
  CHECKL(RankSetCheck(seg->rankSet));
  if (seg->rankSet == RankSetEMPTY) {
    /* <design/seg/#field.rankSet.empty>: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(seg->grey == TraceSetEMPTY);
    CHECKL(seg->sm == AccessSetEMPTY);
    CHECKL(seg->pm == AccessSetEMPTY);
  } else {
    /* <design/seg/#field.rankSet.single>: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(RankSetIsSingle(seg->rankSet));
    /* Can't check barrier invariants because SegCheck is called */
    /* when raising or lowering the barrier. */
    /* .check.wb: If summary isn't universal then it must be */
    /* write shielded. */
    /* CHECKL(seg->_summary == RefSetUNIV || (seg->_sm & AccessWRITE)); */
    /* @@@@ What can be checked about the read barrier? */
  }
  return TRUE;
}


/* segTrivInit -- method to initialize the base fields of a segment */

static Res segTrivInit(Seg seg, Pool pool, Addr base, Size size,
                       Bool reservoirPermit, va_list args)
{
  /* all the initialization happens in SegInit so checks are safe */
  Size align;
  Arena arena;

  AVERT(Seg, seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);
  AVER(AddrIsAligned(base, align));
  AVER(SizeIsAligned(size, align));
  AVER(SegBase(seg) == base);
  AVER(SegSize(seg) == size);
  AVER(SegPool(seg) == pool);
  AVER(BoolCheck(reservoirPermit));
  UNUSED(args);
  return ResOK;
}


/* segTrivFinish -- finish the base fields of a segment */

static void segTrivFinish(Seg seg)
{
  /* all the generic finishing happens in SegFinish  */
  AVERT(Seg, seg);
}


/* segNoSetGrey -- non-method to change the greyness of a segment */

static void segNoSetGrey(Seg seg, TraceSet grey)
{
  AVERT(Seg, seg);
  AVER(TraceSetCheck(grey));
  AVER(seg->rankSet != RankSetEMPTY);
  NOTREACHED;
}


/* segNoSetWhite -- non-method to change the whiteness of a segment */

static void segNoSetWhite(Seg seg, TraceSet white)
{
  AVERT(Seg, seg);
  AVER(TraceSetCheck(white));
  NOTREACHED;
}


/* segNoSetRankSet -- non-method to set the rank set of a segment */

static void segNoSetRankSet(Seg seg, RankSet rankSet)
{
  AVERT(Seg, seg);
  AVER(RankSetCheck(rankSet));
  NOTREACHED;
}


/* segNoSetSummary -- non-method to set the summary of a segment */

static void segNoSetSummary(Seg seg, RefSet summary)
{
  AVERT(Seg, seg);
  UNUSED(summary);
  NOTREACHED;
}


/* segNoSetRankSummary -- non-method to set the rank set & summary */

static void segNoSetRankSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  AVERT(Seg, seg);
  AVER(RankSetCheck(rankSet));
  UNUSED(summary);
  NOTREACHED;
}


/* segNoBuffer -- non-method to return the buffer of a segment */

static Buffer segNoBuffer(Seg seg)
{
  AVERT(Seg, seg);
  NOTREACHED;
  return NULL;
}


/* segNoSetBuffer -- non-method to set the buffer of a segment */

static void segNoSetBuffer(Seg seg, Buffer buffer)
{
  AVERT(Seg, seg);
  if (buffer != NULL)
    AVERT(Buffer, buffer);
  NOTREACHED;
}



/* segNoMerge -- merge method for segs which don't support merge */

static Res segNoMerge(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit,
                      Bool withReservoirPermit, va_list args)
{
  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == mid);
  AVER(SegBase(segHi) == mid);
  AVER(SegLimit(segHi) == limit);
  AVER(BoolCheck(withReservoirPermit));
  UNUSED(args);
  NOTREACHED;
  return ResFAIL;
}


/* segTrivMerge -- Basic Seg merge method
 *
 * .similar: Segments must be "sufficiently similar".
 * See <design/seg/#merge.inv.similar>
 */

static Res segTrivMerge(Seg seg, Seg segHi,
                        Addr base, Addr mid, Addr limit,
                        Bool withReservoirPermit, va_list args)
{
  Pool pool;
  Size align;
  Arena arena;
  Tract tract;
  Addr addr;

  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  pool = SegPool(seg);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);
  AVER(AddrIsAligned(base, align));
  AVER(AddrIsAligned(mid, align));
  AVER(AddrIsAligned(limit, align));
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == mid);
  AVER(SegBase(segHi) == mid);
  AVER(SegLimit(segHi) == limit);
  AVER(BoolCheck(withReservoirPermit));
  UNUSED(args);

  /* .similar.  */
  AVER(seg->rankSet == segHi->rankSet);
  AVER(seg->white == segHi->white);
  AVER(seg->nailed == segHi->nailed);
  AVER(seg->grey == segHi->grey);
  AVER(seg->pm == segHi->pm);
  AVER(seg->sm == segHi->sm);
  AVER(seg->depth == segHi->depth);
  /* Neither segment may be exposed, or in the shield cache */
  /* See <design/seg/#split-merge.shield> & <code/shield.c#def.depth> */
  AVER(seg->depth == 0);

  /* no need to update fields which match. See .similar */

  seg->limit = limit;
  TRACT_FOR(tract, addr, arena, mid, limit) {
    AVER(TractCheck(tract));  /* <design/check/#type.no-sig> */
    AVER(TractHasSeg(tract));
    AVER(segHi == TractP(tract));
    AVER(TractPool(tract) == pool);
    TRACT_SET_SEG(tract, seg);
  }
  AVER(addr == seg->limit);

  /* Finish segHi. */
  RingRemove(SegPoolRing(segHi));
  RingFinish(SegPoolRing(segHi));
  segHi->sig = SigInvalid;

  AVERT(Seg, seg);
  return ResOK;
}


/* segNoSplit -- split method for segs which don't support splitting */

static Res segNoSplit(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit,
                      Bool withReservoirPermit, va_list args)
{
  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);
  AVER(BoolCheck(withReservoirPermit));
  UNUSED(args);
  NOTREACHED;
  return ResFAIL;

}


/* segTrivSplit -- Basic Seg split method */

static Res segTrivSplit(Seg seg, Seg segHi,
                        Addr base, Addr mid, Addr limit,
                        Bool withReservoirPermit, va_list args)
{
  Tract tract;
  Pool pool;
  Addr addr;
  Size align;
  Arena arena;

  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  pool = SegPool(seg);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);
  AVER(AddrIsAligned(base, align));
  AVER(AddrIsAligned(mid, align));
  AVER(AddrIsAligned(limit, align));
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);
  AVER(BoolCheck(withReservoirPermit));
  UNUSED(args);

  /* Segment may not be exposed, or in the shield cache */
  /* See <design/seg/#split-merge.shield> & <code/shield.c#def.depth> */
  AVER(seg->depth == 0);
 
  /* Full initialization for segHi. Just modify seg. */
  seg->limit = mid;
  segHi->limit = limit;
  segHi->rankSet = seg->rankSet;
  segHi->white = seg->white;
  segHi->nailed = seg->nailed;
  segHi->grey = seg->grey;
  segHi->pm = seg->pm;
  segHi->sm = seg->sm;
  segHi->depth = seg->depth;
  segHi->firstTract = NULL;
  segHi->class = seg->class;
  segHi->sig = SegSig;
  RingInit(SegPoolRing(segHi));

  TRACT_FOR(tract, addr, arena, mid, limit) {
    AVER(TractCheck(tract));  /* <design/check/#type.no-sig> */
    AVER(TractHasSeg(tract));
    AVER(seg == TractP(tract));
    AVER(TractPool(tract) == pool);
    TRACT_SET_SEG(tract, segHi);
    if (addr == mid) {
      AVER(segHi->firstTract == NULL);
      segHi->firstTract = tract;
    }
    AVER(segHi->firstTract != NULL);
  }
  AVER(addr == segHi->limit);

  RingAppend(&pool->segRing, SegPoolRing(segHi));
  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  return ResOK;
}


/* segTrivDescribe -- Basic Seg description method */

static Res segTrivDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;

  if (!TESTT(Seg, seg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "  shield depth $U\n", (WriteFU)seg->depth,
               "  protection mode:",
               NULL);
  if (res != ResOK) return res;
  if (SegPM(seg) & AccessREAD) {
     res = WriteF(stream, " read", NULL);
     if (res != ResOK) return res;
  }
  if (SegPM(seg) & AccessWRITE) {
     res = WriteF(stream, " write", NULL);
     if (res != ResOK) return res;
  }
  res = WriteF(stream, "\n  shield mode:", NULL);
  if (res != ResOK) return res;
  if (SegSM(seg) & AccessREAD) {
     res = WriteF(stream, " read", NULL);
     if (res != ResOK) return res;
  }
  if (SegSM(seg) & AccessWRITE) {
     res = WriteF(stream, " write", NULL);
     if (res != ResOK) return res;
  }
  res = WriteF(stream, "\n  ranks:", NULL);
  /* This bit ought to be in a RankSetDescribe in ref.c. */
  if (RankSetIsMember(seg->rankSet, RankAMBIG)) {
     res = WriteF(stream, " ambiguous", NULL);
     if (res != ResOK) return res;
  }
  if (RankSetIsMember(seg->rankSet, RankEXACT)) {
     res = WriteF(stream, " exact", NULL);
     if (res != ResOK) return res;
  }
  if (RankSetIsMember(seg->rankSet, RankFINAL)) {
     res = WriteF(stream, " final", NULL);
     if (res != ResOK) return res;
  }
  if (RankSetIsMember(seg->rankSet, RankWEAK)) {
     res = WriteF(stream, " weak", NULL);
     if (res != ResOK) return res;
  }
  res = WriteF(stream, "\n",
               "  white  $B\n", (WriteFB)seg->white,
               "  grey   $B\n", (WriteFB)seg->grey,
               "  nailed $B\n", (WriteFB)seg->nailed,
               NULL);
  return res;
}


/* Class GCSeg -- Segment class with GC support
 */


/* GCSegCheck -- check the integrity of a GCSeg */

Bool GCSegCheck(GCSeg gcseg)
{
  Seg seg;
  CHECKS(GCSeg, gcseg);
  seg = &gcseg->segStruct;
  CHECKL(SegCheck(seg));

  if (gcseg->buffer != NULL) {
    CHECKU(Buffer, gcseg->buffer);
    /* <design/seg/#field.buffer.owner> */
    CHECKL(BufferPool(gcseg->buffer) == SegPool(seg));
    CHECKL(BufferRankSet(gcseg->buffer) == SegRankSet(seg));
  }

  /* The segment should be on a grey ring if and only if it is grey. */
  CHECKL(RingCheck(&gcseg->greyRing));
  CHECKL((seg->grey == TraceSetEMPTY) ==
         RingIsSingle(&gcseg->greyRing));

  if (seg->rankSet == RankSetEMPTY) {
    /* <design/seg/#field.rankSet.empty> */
    CHECKL(gcseg->summary == RefSetEMPTY);
  }

  return TRUE;
}


/* gcSegInit -- method to initialize a GC segment */

static Res gcSegInit(Seg seg, Pool pool, Addr base, Size size,
                     Bool withReservoirPermit, va_list args)
{
  SegClass super;
  GCSeg gcseg;
  Arena arena;
  Align align;
  Res res;

  AVERT(Seg, seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);
  AVER(AddrIsAligned(base, align));
  AVER(SizeIsAligned(size, align));
  gcseg = SegGCSeg(seg);
  AVER(&gcseg->segStruct == seg);
  AVER(BoolCheck(withReservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(GCSegClass);
  res = super->init(seg, pool, base, size, withReservoirPermit, args);
  if (ResOK != res)
    return res;

  gcseg->summary = RefSetEMPTY;
  gcseg->buffer = NULL;
  RingInit(&gcseg->greyRing);
  gcseg->sig = GCSegSig;

  AVERT(GCSeg, gcseg);
  return ResOK;
}


/* gcSegFinish -- finish a GC segment */

static void gcSegFinish(Seg seg)
{
  SegClass super;
  GCSeg gcseg;

  AVERT(Seg, seg);
  gcseg = SegGCSeg(seg);
  AVERT(GCSeg, gcseg);
  AVER(&gcseg->segStruct == seg);

  if (SegGrey(seg) != TraceSetEMPTY) {
    RingRemove(&gcseg->greyRing);
    seg->grey = TraceSetEMPTY;
  }
  gcseg->summary = RefSetEMPTY;

  gcseg->sig = SigInvalid;

  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(gcseg->buffer == NULL);

  RingFinish(&gcseg->greyRing);

  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(GCSegClass);
  super->finish(seg);
}


/* gcSegSetGreyInternal -- change the greyness of a segment
 *
 * Internal method for updating the greyness of a GCSeg.
 * Updates the grey ring and the grey seg count.
 * Doesn't affect the shield (so it can be used by split
 * & merge methods).
 */

static void gcSegSetGreyInternal(Seg seg, TraceSet oldGrey, TraceSet grey)
{
  GCSeg gcseg;
  Arena arena;
  Rank rank;
 
  /* Internal method. Parameters are checked by caller */
  gcseg = SegGCSeg(seg);
  arena = PoolArena(SegPool(seg));
  seg->grey = grey;

  /* If the segment is now grey and wasn't before, add it to the */
  /* appropriate grey list so that TraceFindGrey can locate it */
  /* quickly later.  If it is no longer grey and was before, */
  /* remove it from the list. */
  if (oldGrey == TraceSetEMPTY) {
    if (grey != TraceSetEMPTY) {
      AVER(RankSetIsSingle(seg->rankSet));
      for(rank = 0; rank < RankLIMIT; ++rank)
        if (RankSetIsMember(seg->rankSet, rank)) {
          /* NOTE: We push the segment onto the front of the queue, so that
             we preserve some locality of scanning, and so that we tend to
             forward objects that are closely linked to the same or nearby
             segments. */
          RingInsert(ArenaGreyRing(arena, rank), &gcseg->greyRing);
          break;
        }
      AVER(rank != RankLIMIT); /* there should've been a match */
    }
  } else {
    if (grey == TraceSetEMPTY)
      RingRemove(&gcseg->greyRing);
  }

  STATISTIC_STAT
    ({
       TraceId ti; Trace trace;
       TraceSet diff;

       diff = TraceSetDiff(grey, oldGrey);
       TRACE_SET_ITER(ti, trace, diff, arena)
         ++trace->greySegCount;
         if (trace->greySegCount > trace->greySegMax)
           trace->greySegMax = trace->greySegCount;
       TRACE_SET_ITER_END(ti, trace, diff, arena);

       diff = TraceSetDiff(oldGrey, grey);
       TRACE_SET_ITER(ti, trace, diff, arena)
         --trace->greySegCount;
       TRACE_SET_ITER_END(ti, trace, diff, arena);
     });

}


/* gcSegSetGrey -- GCSeg method to change the greyness of a segment
 *
 * Sets the segment greyness to the trace set grey and adjusts
 * the shielding on the segment appropriately.
 */

static void gcSegSetGrey(Seg seg, TraceSet grey)
{
  GCSeg gcseg;
  TraceSet oldGrey, flippedTraces;
  Arena arena;
 
  AVERT_CRITICAL(Seg, seg);            /* .seg.method.check */
  AVER_CRITICAL(TraceSetCheck(grey));  /* .seg.method.check */
  AVER(seg->rankSet != RankSetEMPTY);
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);
  UNUSED(gcseg);

  arena = PoolArena(SegPool(seg));
  oldGrey = seg->grey;
  gcSegSetGreyInternal(seg, oldGrey, grey); /* do the work */

  /* The read barrier is raised when the segment is grey for */
  /* some _flipped_ trace, i.e., is grey for a trace for which */
  /* the mutator is black. */
  flippedTraces = arena->flippedTraces;
  if (TraceSetInter(oldGrey, flippedTraces) == TraceSetEMPTY) {
    if (TraceSetInter(grey, flippedTraces) != TraceSetEMPTY)
      ShieldRaise(arena, seg, AccessREAD);
  } else {
    if (TraceSetInter(grey, flippedTraces) == TraceSetEMPTY)
      ShieldLower(arena, seg, AccessREAD);
  }

  EVENT3(SegSetGrey, arena, seg, grey);
}


/* gcSegSetWhite -- GCSeg method to change whiteness of a segment
 *
 * Sets the segment whiteness to the trace set ts.
 */

static void gcSegSetWhite(Seg seg, TraceSet white)
{
  GCSeg gcseg;
  Tract tract;
  Arena arena;
  Addr addr, limit;

  AVERT_CRITICAL(Seg, seg);            /* .seg.method.check */
  AVER_CRITICAL(TraceSetCheck(white)); /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  AVERT_CRITICAL(Arena, arena);
  limit = SegLimit(seg);
  /* Each tract of the segment records white traces */
  TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, limit) {
    Seg trseg = NULL; /* suppress compiler warning */

    AVER_CRITICAL(TractCheck(tract));  /* <design/check/#type.no-sig> */
    AVER_CRITICAL(TRACT_SEG(&trseg, tract) && (trseg == seg));
    TractSetWhite(tract, white);
  }
  AVER(addr == limit);

  seg->white = white;
}


/* gcSegSetRankSet -- GCSeg method to set the rank set of a segment
 *
 * If the rank set is made non-empty then the segment's summary is
 * now a subset of the mutator's (which is assumed to be RefSetUNIV)
 * so the write barrier must be imposed on the segment.  If the
 * rank set is made empty then there are no longer any references
 * on the segment so the barrier is removed.
 *
 * The caller must set the summary to empty before setting the rank
 * set to empty.  The caller must set the rank set to non-empty before
 * setting the summary to non-empty.
 */

static void gcSegSetRankSet(Seg seg, RankSet rankSet)
{
  GCSeg gcseg;
  RankSet oldRankSet;
  Arena arena;

  AVERT_CRITICAL(Seg, seg);                /* .seg.method.check */
  AVER_CRITICAL(RankSetCheck(rankSet));    /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet)); /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  oldRankSet = seg->rankSet;
  seg->rankSet = rankSet;

  if (oldRankSet == RankSetEMPTY) {
    if (rankSet != RankSetEMPTY) {
      AVER(gcseg->summary == RefSetEMPTY);
      ShieldRaise(arena, seg, AccessWRITE);
    }
  } else {
    if (rankSet == RankSetEMPTY) {
      AVER(gcseg->summary == RefSetEMPTY);
      ShieldLower(arena, seg, AccessWRITE);
    }
  }
}


/* gcSegSetSummary -- GCSeg method to change the summary on a segment
 *
 * In fact, we only need to raise the write barrier if the
 * segment contains references, and its summary is strictly smaller
 * than the summary of the unprotectable data (i.e. the mutator).
 * We don't maintain such a summary, assuming that the mutator can
 * access all references, so its summary is RefSetUNIV.
 */

static void gcSegSetSummary(Seg seg, RefSet summary)
{
  GCSeg gcseg;
  RefSet oldSummary;
  Arena arena;

  AVERT_CRITICAL(Seg, seg);                 /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  oldSummary = gcseg->summary;
  gcseg->summary = summary;

  AVER(seg->rankSet != RankSetEMPTY);

  /* Note: !RefSetSuper is a test for a strict subset */
  if (!RefSetSuper(summary, RefSetUNIV)) {
    if (RefSetSuper(oldSummary, RefSetUNIV))
      ShieldRaise(arena, seg, AccessWRITE);
  } else {
    if (!RefSetSuper(oldSummary, RefSetUNIV))
      ShieldLower(arena, seg, AccessWRITE);
  }
}


/* gcSegSetRankSummary -- GCSeg method to set both rank set and summary */

static void gcSegSetRankSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  GCSeg gcseg;
  Bool wasShielded, willbeShielded;
  Arena arena;

  AVERT_CRITICAL(Seg, seg);                    /* .seg.method.check */
  AVER_CRITICAL(RankSetCheck(rankSet));        /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet));  /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  /* rankSet == RankSetEMPTY implies summary == RefSetEMPTY */
  AVER(rankSet != RankSetEMPTY || summary == RefSetEMPTY);

  arena = PoolArena(SegPool(seg));

  wasShielded = (seg->rankSet != RankSetEMPTY && gcseg->summary != RefSetUNIV);
  willbeShielded = (rankSet != RankSetEMPTY && summary != RefSetUNIV);

  seg->rankSet = rankSet;
  gcseg->summary = summary;

  if (willbeShielded && !wasShielded) {
    ShieldRaise(arena, seg, AccessWRITE);
  } else if (wasShielded && !willbeShielded) {
    ShieldLower(arena, seg, AccessWRITE);
  }
}


/* gcSegBuffer -- GCSeg method to return the buffer of a segment */

static Buffer gcSegBuffer(Seg seg)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);               /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);           /* .seg.method.check */
  AVER_CRITICAL(&gcseg->segStruct == seg);

  return gcseg->buffer;
}


/* gcSegSetBuffer -- GCSeg method to change the buffer of a segment */

static void gcSegSetBuffer(Seg seg, Buffer buffer)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);              /* .seg.method.check */
  if (buffer != NULL)
    AVERT_CRITICAL(Buffer, buffer);
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  gcseg->buffer = buffer;
}


/* gcSegMerge -- GCSeg merge method
 *
 * .buffer: Can't merge two segments both with buffers.
 * See <design/seg/#merge.inv.buffer>.
 */

static Res gcSegMerge(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit,
                      Bool withReservoirPermit, va_list args)
{
  SegClass super;
  GCSeg gcseg, gcsegHi;
  TraceSet grey;
  RefSet summary;
  Buffer buf;
  Res res;

  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  gcseg = SegGCSeg(seg);
  gcsegHi = SegGCSeg(segHi);
  AVERT(GCSeg, gcseg);
  AVERT(GCSeg, gcsegHi);
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == mid);
  AVER(SegBase(segHi) == mid);
  AVER(SegLimit(segHi) == limit);
  AVER(BoolCheck(withReservoirPermit));

  buf = gcsegHi->buffer;      /* any buffer on segHi must be reassigned */
  AVER(buf == NULL || gcseg->buffer == NULL); /* See .buffer */
  grey = SegGrey(segHi);      /* check greyness */
  AVER(SegGrey(seg) == grey);

  /* Merge the superclass fields via next-method call */
  super = SEG_SUPERCLASS(GCSegClass);
  res = super->merge(seg, segHi, base, mid, limit,
                     withReservoirPermit, args);
  if (res != ResOK)
    goto failSuper;

  /* Update fields of gcseg. Finish gcsegHi. */
  summary = RefSetUnion(gcseg->summary, gcsegHi->summary);
  if (summary != gcseg->summary) {
    gcSegSetSummary(seg, summary);
    /* <design/seg/#split-merge.shield.re-flush> */
    ShieldFlush(PoolArena(SegPool(seg)));
  }

  gcSegSetGreyInternal(segHi, grey, TraceSetEMPTY);
  gcsegHi->summary = RefSetEMPTY;
  gcsegHi->sig = SigInvalid;
  RingFinish(&gcsegHi->greyRing);

  /* Reassign any buffer that was connected to segHi  */
  if (NULL != buf) {
    AVER(gcseg->buffer == NULL);
    gcseg->buffer = buf;
    gcsegHi->buffer = NULL;
    BufferReassignSeg(buf, seg);
  }

  AVERT(GCSeg, gcseg);
  return ResOK;

failSuper:
  AVERT(GCSeg, gcseg);
  AVERT(GCSeg, gcsegHi);
  return res;
}


/* gcSegSplit -- GCSeg split method */

static Res gcSegSplit(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit,
                      Bool withReservoirPermit, va_list args)
{
  SegClass super;
  GCSeg gcseg, gcsegHi;
  Buffer buf;
  TraceSet grey;
  Res res;

  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  gcseg = SegGCSeg(seg);
  gcsegHi = SegGCSeg(segHi);
  AVERT(GCSeg, gcseg);
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);
  AVER(BoolCheck(withReservoirPermit));
 
  grey = SegGrey(seg);
  buf = gcseg->buffer; /* Look for buffer to reassign to segHi */
  if (buf != NULL) {
    if (BufferLimit(buf) > mid) {
      /* Existing buffer extends above the split point */
      AVER(BufferBase(buf) > mid); /* check it's all above the split */
    } else {
      buf = NULL; /* buffer lies below split and is unaffected */
    }
  }   

  /* Split the superclass fields via next-method call */
  super = SEG_SUPERCLASS(GCSegClass);
  res = super->split(seg, segHi, base, mid, limit,
                     withReservoirPermit, args);
  if (res != ResOK)
    goto failSuper;

  /* Full initialization for segHi. */
  gcsegHi->summary = gcseg->summary;
  gcsegHi->buffer = NULL;
  RingInit(&gcsegHi->greyRing);
  gcsegHi->sig = GCSegSig;
  gcSegSetGreyInternal(segHi, TraceSetEMPTY, grey);

  /* Reassign buffer if it's now connected to segHi  */
  if (NULL != buf) {
    gcsegHi->buffer = buf;
    gcseg->buffer = NULL;
    BufferReassignSeg(buf, segHi);
  }

  AVERT(GCSeg, gcseg);
  AVERT(GCSeg, gcsegHi);
  return ResOK;

failSuper:
  AVERT(GCSeg, gcseg);
  return res;
}


/* gcSegDescribe -- GCSeg  description method */

static Res gcSegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  SegClass super;
  GCSeg gcseg;

  if (!TESTT(Seg, seg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  gcseg = SegGCSeg(seg);
  if (!TESTT(GCSeg, gcseg)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(GCSegClass);
  res = super->describe(seg, stream);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  summary $W\n", (WriteFW)gcseg->summary,
               NULL);
  if (res != ResOK) return res;

  if (gcseg->buffer == NULL) {
    res = WriteF(stream, "  buffer: NULL\n", NULL);
  }
  else {
    res = BufferDescribe(gcseg->buffer, stream);
  }
  if (res != ResOK) return res;

  return ResOK;
}


/* SegClassCheck -- check a segment class */

Bool SegClassCheck(SegClass class)
{
  CHECKL(ProtocolClassCheck(&class->protocol));
  CHECKL(class->name != NULL); /* Should be <= 6 char C identifier */
  CHECKL(class->size >= sizeof(SegStruct));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->setGrey));
  CHECKL(FUNCHECK(class->setWhite));
  CHECKL(FUNCHECK(class->setRankSet));
  CHECKL(FUNCHECK(class->setRankSummary));
  CHECKL(FUNCHECK(class->merge));
  CHECKL(FUNCHECK(class->split));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(SegClass, class);
  return TRUE;
}


/* SegClass -- the vanilla segment class definition */

DEFINE_CLASS(SegClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "SEG";
  class->size = sizeof(SegStruct);
  class->init = segTrivInit;
  class->finish = segTrivFinish;
  class->setSummary = segNoSetSummary; 
  class->buffer = segNoBuffer; 
  class->setBuffer = segNoSetBuffer; 
  class->setGrey = segNoSetGrey;
  class->setWhite = segNoSetWhite;
  class->setRankSet = segNoSetRankSet;
  class->setRankSummary = segNoSetRankSummary;
  class->merge = segTrivMerge;
  class->split = segTrivSplit;
  class->describe = segTrivDescribe;
  class->sig = SegClassSig;
}


/* GCSegClass -- GC-supporting segment class definition */

typedef SegClassStruct GCSegClassStruct;

DEFINE_CLASS(GCSegClass, class)
{
  INHERIT_CLASS(class, SegClass);
  class->name = "GCSEG";
  class->size = sizeof(GCSegStruct);
  class->init = gcSegInit;
  class->finish = gcSegFinish;
  class->setSummary = gcSegSetSummary; 
  class->buffer = gcSegBuffer; 
  class->setBuffer = gcSegSetBuffer; 
  class->setGrey = gcSegSetGrey;
  class->setWhite = gcSegSetWhite;
  class->setRankSet = gcSegSetRankSet;
  class->setRankSummary = gcSegSetRankSummary;
  class->merge = gcSegMerge;
  class->split = gcSegSplit;
  class->describe = gcSegDescribe;
}


/* SegClassMixInNoSplitMerge -- Mix-in for unsupported merge
 *
 * Classes which don't support segment splitting and merging
 * may mix this in to ensure that erroneous calls are checked.
 */

void SegClassMixInNoSplitMerge(SegClass class)
{
  /* Can't check class because it's not initialized yet */
  class->merge = segNoMerge;
  class->split = segNoSplit;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

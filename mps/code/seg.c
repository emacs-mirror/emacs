/* seg.c: SEGMENTS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
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
 */

#include "tract.h"
#include "mpm.h"

SRCID(seg, "$Id$");


/* forward declarations */

static void SegFinish(Seg seg);

static Res SegInit(Seg seg, SegClass klass, Pool pool,
                   Addr base, Size size, ArgList args);


/* Generic interface support */


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegClass klass, LocusPref pref,
             Size size, Pool pool, ArgList args)
{
  Res res;
  Arena arena;
  Seg seg;
  Addr base;
  void *p;

  AVER(segReturn != NULL);
  AVERT(SegClass, klass);
  AVERT(LocusPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsArenaGrains(size, arena));

  /* allocate the memory from the arena */
  res = ArenaAlloc(&base, pref, size, pool);
  if (res != ResOK)
    goto failArena;

  /* allocate the segment object from the control pool */
  res = ControlAlloc(&p, arena, klass->size);
  if (res != ResOK)
    goto failControl;
  seg = p;

  res = SegInit(seg, klass, pool, base, size, args);
  if (res != ResOK)
    goto failInit;

  EVENT5(SegAlloc, arena, seg, SegBase(seg), size, pool);
  *segReturn = seg;
  return ResOK;

failInit:
  ControlFree(arena, seg, klass->size);
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
  Size size, structSize;

  AVERT(Seg, seg);
  pool = SegPool(seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  base = SegBase(seg);
  size = SegSize(seg);
  structSize = ClassOfPoly(Seg, seg)->size;

  SegFinish(seg);
  ControlFree(arena, seg, structSize);
  ArenaFree(base, size, pool);

  EVENT2(SegFree, arena, seg);
}


/* SegInit -- initialize a segment */

static Res SegAbsInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  Arena arena;
  Addr addr, limit;
  Tract tract;
  
  AVER(seg != NULL);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVER(AddrIsArenaGrain(base, arena));
  AVER(SizeIsArenaGrains(size, arena));
  AVERT(ArgList, args);

  NextMethod(Inst, Seg, init)(CouldBeA(Inst, seg));

  limit = AddrAdd(base, size);
  seg->limit = limit;
  seg->rankSet = RankSetEMPTY;
  seg->white = TraceSetEMPTY;
  seg->nailed = TraceSetEMPTY;
  seg->grey = TraceSetEMPTY;
  seg->pm = AccessSetEMPTY;
  seg->sm = AccessSetEMPTY;
  seg->defer = WB_DEFER_INIT;
  seg->depth = 0;
  seg->queued = FALSE;
  seg->firstTract = NULL;
  RingInit(SegPoolRing(seg));
  
  TRACT_FOR(tract, addr, arena, base, limit) {
    AVERT(Tract, tract);
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

  SetClassOfPoly(seg, CLASS(Seg));
  seg->sig = SegSig;
  AVERC(Seg, seg);

  RingAppend(&pool->segRing, SegPoolRing(seg));

  return ResOK;
}

static Res SegInit(Seg seg, SegClass klass, Pool pool, Addr base, Size size, ArgList args)
{
  Res res;

  AVERT(SegClass, klass);

  /* Klass specific initialization comes last */
  res = klass->init(seg, pool, base, size, args);
  if (res != ResOK)
    return res;
   
  AVERT(Seg, seg);

  return ResOK;
}


/* SegFinish -- finish a segment */

static void SegAbsFinish(Inst inst)
{
  Seg seg = MustBeA(Seg, inst);
  Arena arena;
  Addr addr, limit;
  Tract tract;

  AVERT(Seg, seg);

  RingRemove(SegPoolRing(seg));

  arena = PoolArena(SegPool(seg));

  /* TODO: It would be good to avoid deprotecting segments eagerly
     when we free them, especially if they're going to be
     unmapped. This would require tracking of protection independent
     of the existence of a SegStruct. */
  if (seg->sm != AccessSetEMPTY) {
    ShieldLower(arena, seg, seg->sm);
  }

  seg->rankSet = RankSetEMPTY;

  /* See <code/shield.c#shield.flush> */
  AVER(seg->depth == 0);
  if (seg->queued)
    ShieldFlush(PoolArena(SegPool(seg)));
  AVER(!seg->queued);

  limit = SegLimit(seg);
  
  TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, limit) {
    AVERT(Tract, tract);
    TractSetWhite(tract, TraceSetEMPTY);
    TRACT_UNSET_SEG(tract);
  }
  AVER(addr == seg->limit);

  RingFinish(SegPoolRing(seg));

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see <code/shield.c#def.depth>). */
  AVER(seg->depth == 0);
  /* Check not shielded or protected (so that pages in hysteresis */
  /* fund are not protected) */
  AVER(seg->sm == AccessSetEMPTY);
  AVER(seg->pm == AccessSetEMPTY);

  seg->sig = SigInvalid;
  InstFinish(CouldBeA(Inst, seg));
}

static void SegFinish(Seg seg)
{
  AVERC(Seg, seg);
  Method(Inst, seg, finish)(MustBeA(Inst, seg));
}


/* SegSetGrey -- change the greyness of a segment
 *
 * Sets the segment greyness to the trace set grey.
 */

void SegSetGrey(Seg seg, TraceSet grey)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, grey);
  AVER(grey == TraceSetEMPTY || SegRankSet(seg) != RankSetEMPTY);

  /* Don't dispatch to the class method if there's no actual change in
     greyness, or if the segment doesn't contain any references. */
  if (grey != SegGrey(seg) && SegRankSet(seg) != RankSetEMPTY)
    Method(Seg, seg, setGrey)(seg, grey);
}


/* SegSetWhite -- change the whiteness of a segment
 *
 * Sets the segment whiteness to the trace set ts.
 */

void SegSetWhite(Seg seg, TraceSet white)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, white);
  Method(Seg, seg, setWhite)(seg, white);
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
  AVERT(RankSet, rankSet);
  AVER(rankSet != RankSetEMPTY || SegSummary(seg) == RefSetEMPTY);
  Method(Seg, seg, setRankSet)(seg, rankSet);
}


/* SegSetSummary -- change the summary on a segment */

void SegSetSummary(Seg seg, RefSet summary)
{
  AVERT(Seg, seg);
  AVER(summary == RefSetEMPTY || SegRankSet(seg) != RankSetEMPTY);

#if defined(REMEMBERED_SET_NONE)
  /* Without protection, we can't maintain the remembered set because
     there are writes we don't know about. */
  summary = RefSetUNIV;
#endif

  if (summary != SegSummary(seg))
    Method(Seg, seg, setSummary)(seg, summary);
}


/* SegSetRankAndSummary -- set both the rank set and the summary */

void SegSetRankAndSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  AVERT(Seg, seg); 
  AVERT(RankSet, rankSet);

#if defined(REMEMBERED_SET_NONE)
  if (rankSet != RankSetEMPTY) {
    summary = RefSetUNIV;
  }
#endif

  Method(Seg, seg, setRankSummary)(seg, rankSet, summary);
}


/* SegHasBuffer -- segment has a buffer? */

Bool SegHasBuffer(Seg seg)
{
  Buffer buffer;
  return SegBuffer(&buffer, seg);
}


/* SegBuffer -- get the buffer of a segment */

Bool SegBuffer(Buffer *bufferReturn, Seg seg)
{
  AVERT_CRITICAL(Seg, seg);  /* .seg.critical */
  return Method(Seg, seg, buffer)(bufferReturn, seg);
}


/* SegSetBuffer -- change the buffer on a segment */

void SegSetBuffer(Seg seg, Buffer buffer)
{
  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
  Method(Seg, seg, setBuffer)(seg, buffer);
}


/* SegUnsetBuffer -- remove the buffer from a segment */

void SegUnsetBuffer(Seg seg)
{
  AVERT(Seg, seg);
  Method(Seg, seg, unsetBuffer)(seg);
}


/* SegBufferScanLimit -- limit of scannable objects in segment */

Addr SegBufferScanLimit(Seg seg)
{
  Addr limit;
  Buffer buf;

  AVERT(Seg, seg);

  if (!SegBuffer(&buf, seg)) {
    /* Segment is unbuffered: entire segment scannable */
    limit = SegLimit(seg);
  } else {
    /* Segment is buffered: scannable up to limit of initialized objects. */
    limit = BufferScanLimit(buf);
  }
  return limit;
}


/* SegDescribe -- describe a segment */

Res SegAbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Seg seg = CouldBeA(Seg, inst);
  Res res;
  Pool pool;

  if (!TESTC(Seg, seg))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, Seg, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  pool = SegPool(seg);

  res = WriteF(stream, depth + 2,
               "base  $A\n", (WriteFA)SegBase(seg),
               "limit $A\n", (WriteFA)SegLimit(seg),
               "pool  $P ($U)\n", (WriteFP)pool, (WriteFU)pool->serial,
               "depth $U\n", seg->depth,
               "pm",
               seg->pm == AccessSetEMPTY ? " EMPTY" : "",
               seg->pm & AccessREAD ? " READ" : "",
               seg->pm & AccessWRITE ? " WRITE" : "",
               "\n",
               "sm",
               seg->sm == AccessSetEMPTY ? " EMPTY" : "",
               seg->sm & AccessREAD ? " READ" : "",
               seg->sm & AccessWRITE ? " WRITE" : "",
               "\n",
               "grey $B\n", (WriteFB)seg->grey,
               "white $B\n", (WriteFB)seg->white,
               "nailed $B\n", (WriteFB)seg->nailed,
               "rankSet",
               seg->rankSet == RankSetEMPTY ? " EMPTY" : "",
               BS_IS_MEMBER(seg->rankSet, RankAMBIG) ? " AMBIG" : "",
               BS_IS_MEMBER(seg->rankSet, RankEXACT) ? " EXACT" : "",
               BS_IS_MEMBER(seg->rankSet, RankFINAL) ? " FINAL" : "",
               BS_IS_MEMBER(seg->rankSet, RankWEAK)  ? " WEAK"  : "",
               "\n",
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}

Res SegDescribe(Seg seg, mps_lib_FILE *stream, Count depth)
{
  return Method(Inst, seg, describe)(MustBeA(Inst, seg), stream, depth);
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

static Bool PoolFirst(Pool *poolReturn, Arena arena)
{
  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  if (RingIsSingle(ArenaPoolRing(arena)))
    return FALSE;
  *poolReturn = PoolOfArenaRing(RingNext(ArenaPoolRing(arena)));
  return TRUE;
}

static Bool PoolNext(Pool *poolReturn, Arena arena, Pool pool)
{
  /* Was that the last pool? */
  if (RingNext(PoolArenaRing(pool)) == ArenaPoolRing(arena))
    return FALSE;
  *poolReturn = PoolOfArenaRing(RingNext(PoolArenaRing(pool)));
  return TRUE;
}

static Bool PoolWithSegs(Pool *poolReturn, Arena arena, Pool pool)
{
  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Pool, pool);
  
  while (RingIsSingle(PoolSegRing(pool)))
    if (!PoolNext(&pool, arena, pool))
      return FALSE;

  *poolReturn = pool;
  return TRUE;
}

Bool SegFirst(Seg *segReturn, Arena arena)
{
  Pool pool;

  AVER(segReturn != NULL);
  AVERT(Arena, arena);

  if (!PoolFirst(&pool, arena) || /* unlikely, but still... */
      !PoolWithSegs(&pool, arena, pool))
    return FALSE;

  *segReturn = SegOfPoolRing(RingNext(PoolSegRing(pool)));
  return TRUE;
}


/* SegNext -- return the "next" seg in the arena
 *
 * This is used as the iteration step when iterating over all
 * segs in the arena.
 *
 * Pool is the pool of the previous segment, and next is the
 * RingNext(SegPoolRing(seg)) of the previous segment.  This allows for
 * segment deletion during iteration.
 */

Bool SegNextOfRing(Seg *segReturn, Arena arena, Pool pool, Ring next)
{
  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  AVERT_CRITICAL(Arena, arena);
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(Ring, next);
  
  if (next == PoolSegRing(pool)) {
    if (!PoolNext(&pool, arena, pool) ||
        !PoolWithSegs(&pool, arena, pool))
      return FALSE;
    *segReturn = SegOfPoolRing(RingNext(PoolSegRing(pool)));
    return TRUE;
  }
  
  *segReturn = SegOfPoolRing(next);
  return TRUE;
}

Bool SegNext(Seg *segReturn, Arena arena, Seg seg)
{
  AVERT_CRITICAL(Seg, seg);
  return SegNextOfRing(segReturn, arena,
                       SegPool(seg), RingNext(SegPoolRing(seg)));
}


/* SegMerge -- Merge two adjacent segments
 *
 * See <design/seg/#merge>
 */

Res SegMerge(Seg *mergedSegReturn, Seg segLo, Seg segHi)
{
  SegClass klass;
  Addr base, mid, limit;
  Arena arena;
  Res res;

  AVER(NULL != mergedSegReturn);
  AVERT(Seg, segLo);
  AVERT(Seg, segHi);
  klass = ClassOfPoly(Seg, segLo);
  AVER(ClassOfPoly(Seg, segHi) == klass);
  AVER(SegPool(segLo) == SegPool(segHi));
  base = SegBase(segLo);
  mid = SegLimit(segLo);
  limit = SegLimit(segHi);
  AVER(SegBase(segHi) == SegLimit(segLo));
  arena = PoolArena(SegPool(segLo));

  if (segLo->queued || segHi->queued)
    ShieldFlush(arena);  /* see <design/seg/#split-merge.shield> */

  /* Invoke class-specific methods to do the merge */
  res = Method(Seg, segLo, merge)(segLo, segHi, base, mid, limit);
  if (ResOK != res)
    goto failMerge;

  EVENT2(SegMerge, segLo, segHi);
  /* Deallocate segHi object */
  ControlFree(arena, segHi, klass->size);
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

Res SegSplit(Seg *segLoReturn, Seg *segHiReturn, Seg seg, Addr at)
{
  Addr base, limit;
  SegClass klass;
  Seg segNew;
  Arena arena;
  Res res;
  void *p;
  Buffer buffer;

  AVER(NULL != segLoReturn);
  AVER(NULL != segHiReturn);
  AVERT(Seg, seg);
  klass = ClassOfPoly(Seg, seg);
  arena = PoolArena(SegPool(seg));
  base = SegBase(seg);
  limit = SegLimit(seg);
  AVERT(Arena, arena);
  AVER(AddrIsArenaGrain(at, arena));
  AVER(at > base);
  AVER(at < limit);

  /* Can only split a buffered segment if the entire buffer is below
   * the split point. */
  AVER(!SegBuffer(&buffer, seg) || BufferLimit(buffer) <= at);

  if (seg->queued)
    ShieldFlush(arena);  /* see <design/seg/#split-merge.shield> */
  AVER(SegSM(seg) == SegPM(seg));

  /* Allocate the new segment object from the control pool */
  res = ControlAlloc(&p, arena, klass->size);
  if (ResOK != res)
    goto failControl;
  segNew = p;

  /* Invoke class-specific methods to do the split */
  res = Method(Seg, seg, split)(seg, segNew, base, at, limit);
  if (ResOK != res)
    goto failSplit;

  EVENT4(SegSplit, seg, segNew, seg, at);
  AVERT(Seg, seg);
  AVERT(Seg, segNew);
  *segLoReturn = seg;
  *segHiReturn = segNew;
  return ResOK;

failSplit:
  ControlFree(arena, segNew, klass->size);
failControl:
  AVERT(Seg, seg); /* check the original seg is still valid */
  return res;
}


/* SegGreyen -- greyen non-white objects */

void SegGreyen(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  Method(Seg, seg, greyen)(seg, trace);
}


/* SegBlacken -- blacken grey objects without scanning */

void SegBlacken(Seg seg, TraceSet traceSet)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, traceSet);
  Method(Seg, seg, blacken)(seg, traceSet);
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
  Arena arena;
  Pool pool;
 
  CHECKS(Seg, seg);
  CHECKC(Seg, seg);
  CHECKL(TraceSetCheck(seg->white));

  /* can't assume nailed is subset of white - mightn't be during whiten */
  /* CHECKL(TraceSetSub(seg->nailed, seg->white)); */
  CHECKL(TraceSetCheck(seg->grey));
  CHECKD_NOSIG(Tract, seg->firstTract);
  pool = SegPool(seg);
  CHECKU(Pool, pool);
  arena = PoolArena(pool);
  CHECKU(Arena, arena);
  CHECKL(AddrIsArenaGrain(TractBase(seg->firstTract), arena));
  CHECKL(AddrIsArenaGrain(seg->limit, arena));
  CHECKL(seg->limit > TractBase(seg->firstTract));
  /* Can't BoolCheck seg->queued because compilers warn about that on
     single-bit fields. */

  /* Each tract of the segment must agree about white traces. Note
   * that even if the CHECKs are compiled away there is still a
   * significant cost in looping over the tracts, hence the guard. See
   * job003778. */
#if defined(AVER_AND_CHECK_ALL)
  {
    Tract tract;
    Addr addr;
    TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, seg->limit) {
      Seg trseg = NULL; /* suppress compiler warning */

      CHECKD_NOSIG(Tract, tract);
      CHECKL(TRACT_SEG(&trseg, tract));
      CHECKL(trseg == seg);
      CHECKL(TractWhite(tract) == seg->white);
      CHECKL(TractPool(tract) == pool);
    }
    CHECKL(addr == seg->limit);
  }
#endif  /* AVER_AND_CHECK_ALL */

  /* The segment must belong to some pool, so it should be on a */
  /* pool's segment ring.  (Actually, this isn't true just after */
  /* the segment is initialized.) */
  /*  CHECKL(RingNext(&seg->poolRing) != &seg->poolRing); */

  CHECKD_NOSIG(Ring, &seg->poolRing);

  /* Shield invariants -- see design.mps.shield. */
   
  /* The protection mode is never more than the shield mode
     (design.mps.shield.inv.prot.shield). */
  CHECKL(BS_DIFF(seg->pm, seg->sm) == 0);

  /* All unsynced segments have positive depth or are in the queue
     (design.mps.shield.inv.unsynced.depth). */
  CHECKL(seg->sm == seg->pm || seg->depth > 0 || seg->queued);
  
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
    /* TODO: Need gcSegCheck?  What does RankSet imply about being a gcSeg? */
  }
  return TRUE;
}


/* segNoSetGrey -- non-method to change the greyness of a segment */

static void segNoSetGrey(Seg seg, TraceSet grey)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, grey);
  AVER(seg->rankSet != RankSetEMPTY);
  NOTREACHED;
}


/* segNoSetWhite -- non-method to change the whiteness of a segment */

static void segNoSetWhite(Seg seg, TraceSet white)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, white);
  NOTREACHED;
}


/* segNoSetRankSet -- non-method to set the rank set of a segment */

static void segNoSetRankSet(Seg seg, RankSet rankSet)
{
  AVERT(Seg, seg);
  AVERT(RankSet, rankSet);
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
  AVERT(RankSet, rankSet);
  UNUSED(summary);
  NOTREACHED;
}


/* segNoBuffer -- non-method to return the buffer of a segment */

static Bool segNoBuffer(Buffer *bufferReturn, Seg seg)
{
  AVERT(Seg, seg);
  AVER(bufferReturn != NULL);
  NOTREACHED;
  return FALSE;
}


/* segNoSetBuffer -- non-method to set the buffer of a segment */

static void segNoSetBuffer(Seg seg, Buffer buffer)
{
  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
  NOTREACHED;
}


/* segNoSetBuffer -- non-method to set the buffer of a segment */

static void segNoUnsetBuffer(Seg seg)
{
  AVERT(Seg, seg);
  NOTREACHED;
}



/* segNoMerge -- merge method for segs which don't support merge */

static Res segNoMerge(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit)
{
  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == mid);
  AVER(SegBase(segHi) == mid);
  AVER(SegLimit(segHi) == limit);
  NOTREACHED;
  return ResFAIL;
}


/* segTrivMerge -- Basic Seg merge method
 *
 * .similar: Segments must be "sufficiently similar".
 * See <design/seg/#merge.inv.similar>
 */

static Res segTrivMerge(Seg seg, Seg segHi,
                        Addr base, Addr mid, Addr limit)
{
  Pool pool;
  Arena arena;
  Tract tract;
  Addr addr;

  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  pool = SegPool(seg);
  arena = PoolArena(pool);
  AVER(AddrIsArenaGrain(base, arena));
  AVER(AddrIsArenaGrain(mid, arena));
  AVER(AddrIsArenaGrain(limit, arena));
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == mid);
  AVER(SegBase(segHi) == mid);
  AVER(SegLimit(segHi) == limit);

  /* .similar.  */
  AVER(seg->rankSet == segHi->rankSet);
  AVER(seg->white == segHi->white);
  AVER(seg->nailed == segHi->nailed);
  AVER(seg->grey == segHi->grey);
  AVER(seg->pm == segHi->pm);
  AVER(seg->sm == segHi->sm);
  AVER(seg->depth == segHi->depth);
  AVER(seg->queued == segHi->queued);
  /* Neither segment may be exposed, or in the shield cache */
  /* See <design/seg/#split-merge.shield> & <code/shield.c#def.depth> */
  AVER(seg->depth == 0);
  AVER(!seg->queued);

  /* no need to update fields which match. See .similar */

  seg->limit = limit;
  TRACT_FOR(tract, addr, arena, mid, limit) {
    AVERT(Tract, tract);
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
                      Addr base, Addr mid, Addr limit)
{
  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);
  NOTREACHED;
  return ResFAIL;
}


/* segTrivSplit -- Basic Seg split method */

static Res segTrivSplit(Seg seg, Seg segHi,
                        Addr base, Addr mid, Addr limit)
{
  Pool pool = SegPool(MustBeA(Seg, seg));
  Arena arena = PoolArena(pool);
  SegClass klass;
  Tract tract;
  Addr addr;

  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  AVER(AddrIsArenaGrain(base, arena));
  AVER(AddrIsArenaGrain(mid, arena));
  AVER(AddrIsArenaGrain(limit, arena));
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);

  /* Segment may not be exposed, or in the shield queue */
  /* See <design/seg/#split-merge.shield> & <code/shield.c#def.depth> */
  AVER(seg->depth == 0);
  AVER(!seg->queued);
 
  /* Full initialization for segHi. Just modify seg. */
  seg->limit = mid;
  AVERT(Seg, seg);

  InstInit(CouldBeA(Inst, segHi));
  segHi->limit = limit;
  segHi->rankSet = seg->rankSet;
  segHi->white = seg->white;
  segHi->nailed = seg->nailed;
  segHi->grey = seg->grey;
  segHi->pm = seg->pm;
  segHi->sm = seg->sm;
  segHi->depth = seg->depth;
  segHi->queued = seg->queued;
  segHi->firstTract = NULL;
  RingInit(SegPoolRing(segHi));

  TRACT_FOR(tract, addr, arena, mid, limit) {
    AVERT(Tract, tract);
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

  klass = ClassOfPoly(Seg, seg);
  SetClassOfPoly(segHi, klass);
  segHi->sig = SegSig;
  AVERC(Seg, segHi);

  RingAppend(&pool->segRing, SegPoolRing(segHi));

  return ResOK;
}

static void segNoGreyen(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  NOTREACHED;
}

static void segNoBlacken(Seg seg, TraceSet traceSet)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, traceSet);
  NOTREACHED;
}


/* Class GCSeg -- Segment class with GC support
 */


/* GCSegCheck -- check the integrity of a GCSeg */

Bool GCSegCheck(GCSeg gcseg)
{
  Seg seg;
  CHECKS(GCSeg, gcseg);
  seg = &gcseg->segStruct;
  CHECKD(Seg, seg);

  if (gcseg->buffer != NULL) {
    CHECKU(Buffer, gcseg->buffer);
    /* <design/seg/#field.buffer.owner> */
    CHECKL(BufferPool(gcseg->buffer) == SegPool(seg));
    CHECKL(BufferRankSet(gcseg->buffer) == SegRankSet(seg));
  }

  /* The segment should be on a grey ring if and only if it is grey. */
  CHECKD_NOSIG(Ring, &gcseg->greyRing);
  CHECKL((seg->grey == TraceSetEMPTY) ==
         RingIsSingle(&gcseg->greyRing));

  if (seg->rankSet == RankSetEMPTY) {
    /* <design/seg/#field.rankSet.empty> */
    CHECKL(gcseg->summary == RefSetEMPTY);
  }

  CHECKD_NOSIG(Ring, &gcseg->genRing);

  return TRUE;
}


/* gcSegInit -- method to initialize a GC segment */

static Res gcSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  GCSeg gcseg;
  Res res;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, GCSeg, init)(seg, pool, base, size, args);
  if (ResOK != res)
    return res;
  gcseg = CouldBeA(GCSeg, seg);

  gcseg->summary = RefSetEMPTY;
  gcseg->buffer = NULL;
  RingInit(&gcseg->greyRing);
  RingInit(&gcseg->genRing);

  SetClassOfPoly(seg, CLASS(GCSeg));
  gcseg->sig = GCSegSig;
  AVERC(GCSeg, gcseg);

  return ResOK;
}


/* gcSegFinish -- finish a GC segment */

static void gcSegFinish(Inst inst)
{
  Seg seg = MustBeA(Seg, inst);
  GCSeg gcseg = MustBeA(GCSeg, seg);

  if (SegGrey(seg) != TraceSetEMPTY) {
    RingRemove(&gcseg->greyRing);
    seg->grey = TraceSetEMPTY;
  }
  gcseg->summary = RefSetEMPTY;

  gcseg->sig = SigInvalid;

  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(gcseg->buffer == NULL); /* <design/check/#.common> */

  RingFinish(&gcseg->greyRing);
  RingFinish(&gcseg->genRing);

  /* finish the superclass fields last */
  NextMethod(Inst, GCSeg, finish)(inst);
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
  seg->grey = BS_BITFIELD(Trace, grey);

  /* If the segment is now grey and wasn't before, add it to the */
  /* appropriate grey list so that TraceFindGrey can locate it */
  /* quickly later.  If it is no longer grey and was before, */
  /* remove it from the list. */
  if (oldGrey == TraceSetEMPTY) {
    if (grey != TraceSetEMPTY) {
      AVER(RankSetIsSingle(seg->rankSet));
      for(rank = RankMIN; rank < RankLIMIT; ++rank)
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

  STATISTIC({
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
  AVERT_CRITICAL(TraceSet, grey);      /* .seg.method.check */
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
  AVERT_CRITICAL(TraceSet, white);     /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  AVERT_CRITICAL(Arena, arena);
  limit = SegLimit(seg);
  /* Each tract of the segment records white traces */
  TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, limit) {
    Seg trseg = NULL; /* suppress compiler warning */

    AVERT_CRITICAL(Tract, tract);
    AVER_CRITICAL(TRACT_SEG(&trseg, tract));
    AVER_CRITICAL(trseg == seg);
    TractSetWhite(tract, BS_BITFIELD(Trace, white));
  }
  AVER(addr == limit);

  seg->white = BS_BITFIELD(Trace, white);
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
  AVERT_CRITICAL(RankSet, rankSet);        /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet)); /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  oldRankSet = seg->rankSet;
  seg->rankSet = BS_BITFIELD(Rank, rankSet);

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


static void gcSegSyncWriteBarrier(Seg seg, Arena arena)
{
  /* Can't check seg -- this function enforces invariants tested by SegCheck. */
  if (SegSummary(seg) == RefSetUNIV)
    ShieldLower(arena, seg, AccessWRITE);
  else
    ShieldRaise(arena, seg, AccessWRITE);
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
  Arena arena;

  AVERT_CRITICAL(Seg, seg);                 /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  arena = PoolArena(SegPool(seg));
  gcseg->summary = summary;

  AVER(seg->rankSet != RankSetEMPTY);

  gcSegSyncWriteBarrier(seg, arena);
}


/* gcSegSetRankSummary -- GCSeg method to set both rank set and summary */

static void gcSegSetRankSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  GCSeg gcseg;
  Arena arena;

  AVERT_CRITICAL(Seg, seg);                    /* .seg.method.check */
  AVERT_CRITICAL(RankSet, rankSet);            /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet));  /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  /* rankSet == RankSetEMPTY implies summary == RefSetEMPTY */
  AVER(rankSet != RankSetEMPTY || summary == RefSetEMPTY);

  arena = PoolArena(SegPool(seg));

  seg->rankSet = BS_BITFIELD(Rank, rankSet);
  gcseg->summary = summary;

  if (rankSet != RankSetEMPTY)
    gcSegSyncWriteBarrier(seg, arena);
}


/* gcSegBuffer -- GCSeg method to return the buffer of a segment */

static Bool gcSegBuffer(Buffer *bufferReturn, Seg seg)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);               /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);           /* .seg.method.check */
  AVER_CRITICAL(&gcseg->segStruct == seg);

  if (gcseg->buffer != NULL) {
    *bufferReturn = gcseg->buffer;
    return TRUE;
  }

  return FALSE;
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


/* gcSegUnsetBuffer -- GCSeg method to remove the buffer from a segment */

static void gcSegUnsetBuffer(Seg seg)
{
  GCSeg gcseg = MustBeA_CRITICAL(GCSeg, seg); /* .seg.method.check */
  gcseg->buffer = NULL;
}


/* gcSegMerge -- GCSeg merge method
 *
 * .buffer: Can't merge two segments both with buffers.
 * See <design/seg/#merge.inv.buffer>.
 */

static Res gcSegMerge(Seg seg, Seg segHi,
                      Addr base, Addr mid, Addr limit)
{
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

  buf = gcsegHi->buffer;      /* any buffer on segHi must be reassigned */
  AVER(buf == NULL || gcseg->buffer == NULL); /* See .buffer */
  grey = SegGrey(segHi);      /* check greyness */
  AVER(SegGrey(seg) == grey);

  /* Assume that the write barrier shield is being used to implement
     the remembered set only, and so we can merge the shield and
     protection modes by unioning the segment summaries.  See also
     design.mps.seg.merge.inv.similar. */
  summary = RefSetUnion(gcseg->summary, gcsegHi->summary);
  SegSetSummary(seg, summary);
  SegSetSummary(segHi, summary);
  AVER(SegSM(seg) == SegSM(segHi));
  if (SegPM(seg) != SegPM(segHi)) {
    /* This shield won't cope with a partially-protected segment, so
       flush the shield queue to bring both halves in sync.  See also
       design.mps.seg.split-merge.shield.re-flush. */
    ShieldFlush(PoolArena(SegPool(seg)));
  }

  /* Merge the superclass fields via next-method call */
  res = NextMethod(Seg, GCSeg, merge)(seg, segHi, base, mid, limit);
  if (res != ResOK)
    goto failSuper;

  /* Update fields of gcseg. Finish gcsegHi. */
  gcSegSetGreyInternal(segHi, grey, TraceSetEMPTY);
  gcsegHi->summary = RefSetEMPTY;
  gcsegHi->sig = SigInvalid;
  RingFinish(&gcsegHi->greyRing);
  RingRemove(&gcsegHi->genRing);
  RingFinish(&gcsegHi->genRing);

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
                      Addr base, Addr mid, Addr limit)
{
  GCSeg gcseg, gcsegHi;
  Buffer buf;
  TraceSet grey;
  Res res;

  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  gcseg = SegGCSeg(seg);
  AVERT(GCSeg, gcseg);
  AVER(base < mid);
  AVER(mid < limit);
  AVER(SegBase(seg) == base);
  AVER(SegLimit(seg) == limit);
 
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
  res = NextMethod(Seg, GCSeg, split)(seg, segHi, base, mid, limit);
  if (res != ResOK)
    goto failSuper;

  /* Full initialization for segHi. */
  gcsegHi = SegGCSeg(segHi);
  gcsegHi->summary = gcseg->summary;
  gcsegHi->buffer = NULL;
  RingInit(&gcsegHi->greyRing);
  RingInit(&gcsegHi->genRing);
  RingInsert(&gcseg->genRing, &gcsegHi->genRing);
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


/* gcSegGreyen -- GCSeg greyen method
 *
 * If we had a (partially) white segment, then other parts of the same
 * segment might need to get greyed. In fact, all current pools only
 * ever whiten a whole segment, so we never need to greyen any part of
 * an already whitened segment. So we exclude white segments.
 */

static void gcSegGreyen(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);

  if (!TraceSetIsMember(SegWhite(seg), trace))
    SegSetGrey(seg, TraceSetSingle(trace));
}


/* gcSegTrivBlacken -- GCSeg trivial blacken method
 *
 * For segments which do not keep additional colour information.
 */

static void gcSegTrivBlacken(Seg seg, TraceSet traceSet)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, traceSet);
  NOOP;
}


/* gcSegDescribe -- GCSeg  description method */

static Res gcSegDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  GCSeg gcseg = CouldBeA(GCSeg, inst);
  Res res;

  if (!TESTC(GCSeg, gcseg))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  /* Describe the superclass fields first via next-method call */
  res = NextMethod(Inst, GCSeg, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "summary $W\n", (WriteFW)gcseg->summary,
               NULL);
  if (res != ResOK)
    return res;

  if (gcseg->buffer == NULL) {
    res = WriteF(stream, depth + 2, "buffer: NULL\n", NULL);
  } else {
    res = BufferDescribe(gcseg->buffer, stream, depth + 2);
  }
  if (res != ResOK)
    return res;

  return ResOK;
}


/* SegClassCheck -- check a segment class */

Bool SegClassCheck(SegClass klass)
{
  CHECKD(InstClass, &klass->instClassStruct);
  CHECKL(klass->size >= sizeof(SegStruct));
  CHECKL(FUNCHECK(klass->init));
  CHECKL(FUNCHECK(klass->setGrey));
  CHECKL(FUNCHECK(klass->setWhite));
  CHECKL(FUNCHECK(klass->setRankSet));
  CHECKL(FUNCHECK(klass->setRankSummary));
  CHECKL(FUNCHECK(klass->merge));
  CHECKL(FUNCHECK(klass->split));
  CHECKL(FUNCHECK(klass->blacken));
  CHECKS(SegClass, klass);
  return TRUE;
}


/* SegClass -- the vanilla segment class definition */

DEFINE_CLASS(Inst, SegClass, klass)
{
  INHERIT_CLASS(klass, SegClass, InstClass);
}

DEFINE_CLASS(Seg, Seg, klass)
{
  INHERIT_CLASS(&klass->instClassStruct, Seg, Inst);
  klass->instClassStruct.describe = SegAbsDescribe;
  klass->instClassStruct.finish = SegAbsFinish;
  klass->size = sizeof(SegStruct);
  klass->init = SegAbsInit;
  klass->setSummary = segNoSetSummary; 
  klass->buffer = segNoBuffer; 
  klass->setBuffer = segNoSetBuffer;
  klass->unsetBuffer = segNoUnsetBuffer;
  klass->setGrey = segNoSetGrey;
  klass->setWhite = segNoSetWhite;
  klass->setRankSet = segNoSetRankSet;
  klass->setRankSummary = segNoSetRankSummary;
  klass->merge = segTrivMerge;
  klass->split = segTrivSplit;
  klass->greyen = segNoGreyen;
  klass->blacken = segNoBlacken;
  klass->sig = SegClassSig;
  AVERT(SegClass, klass);
}


/* GCSegClass -- GC-supporting segment class definition */

typedef SegClassStruct GCSegClassStruct;

DEFINE_CLASS(Seg, GCSeg, klass)
{
  INHERIT_CLASS(klass, GCSeg, Seg);
  klass->instClassStruct.describe = gcSegDescribe;
  klass->instClassStruct.finish = gcSegFinish;
  klass->size = sizeof(GCSegStruct);
  klass->init = gcSegInit;
  klass->setSummary = gcSegSetSummary; 
  klass->buffer = gcSegBuffer; 
  klass->setBuffer = gcSegSetBuffer;
  klass->unsetBuffer = gcSegUnsetBuffer;
  klass->setGrey = gcSegSetGrey;
  klass->setWhite = gcSegSetWhite;
  klass->setRankSet = gcSegSetRankSet;
  klass->setRankSummary = gcSegSetRankSummary;
  klass->merge = gcSegMerge;
  klass->split = gcSegSplit;
  klass->greyen = gcSegGreyen;
  klass->blacken = gcSegTrivBlacken;
  AVERT(SegClass, klass);
}


/* SegClassMixInNoSplitMerge -- Mix-in for unsupported merge
 *
 * Classes which don't support segment splitting and merging
 * may mix this in to ensure that erroneous calls are checked.
 */

void SegClassMixInNoSplitMerge(SegClass klass)
{
  /* Can't check class because it's not initialized yet */
  klass->merge = segNoMerge;
  klass->split = segNoSplit;
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

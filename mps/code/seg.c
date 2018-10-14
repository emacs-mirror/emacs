/* seg.c: SEGMENTS
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
 *
 * .design: The design for this module is <design/seg/>.
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the generic segment
 * interface and the segment classes Seg, GCSeg and MutatorSeg.
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
  EVENT4(SegAllocFail, arena, size, pool, (unsigned)res);
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

static Res segAbsInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
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
    AVER(!TractHasSeg(tract));
    AVER(TractPool(tract) == pool);
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

static void segAbsFinish(Inst inst)
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

  EVENT3(SegSetGrey, PoolArena(SegPool(seg)), seg, grey);
}


/* SegFlip -- update barriers for trace that's about to flip */

void SegFlip(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);

  /* Don't dispatch to the class method unless the segment is grey for
     the trace that's about to flip, and contains references. */
  if (TraceSetIsMember(SegGrey(seg), trace) && SegRankSet(seg) != RankSetEMPTY)
    Method(Seg, seg, flip)(seg, trace);
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


/* SegBufferFill -- allocate to a buffer from a segment */

Bool SegBufferFill(Addr *baseReturn, Addr *limitReturn, Seg seg, Size size,
                   RankSet rankSet)
{
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Seg, seg);
  AVER(size > 0);
  AVERT(RankSet, rankSet);
  return Method(Seg, seg, bufferFill)(baseReturn, limitReturn,
                                      seg, size, rankSet);
}


/* SegDescribe -- describe a segment */

static Res segAbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
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


/* SegAccess -- mutator read/write access to a segment */

Res SegAccess(Seg seg, Arena arena, Addr addr,
              AccessSet mode, MutatorContext context)
{
  AVERT(Seg, seg);
  AVERT(Arena, arena);
  AVER(arena == PoolArena(SegPool(seg)));
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);

  return Method(Seg, seg, access)(seg, arena, addr, mode, context);
}


/* SegWhiten -- whiten objects */

Res SegWhiten(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  return Method(Seg, seg, whiten)(seg, trace);
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


/* SegScan -- scan a segment */

Res SegScan(Bool *totalReturn, Seg seg, ScanState ss)
{
  AVER(totalReturn != NULL);
  AVERT(Seg, seg);
  AVERT(ScanState, ss);
  AVER(PoolArena(SegPool(seg)) == ss->arena);

  /* We check that either ss->rank is in the segment's
   * ranks, or that ss->rank is exact.  The check is more complicated if
   * we actually have multiple ranks in a seg.
   * See <code/trace.c#scan.conservative> */
  AVER(ss->rank == RankEXACT || RankSetIsMember(SegRankSet(seg), ss->rank));

  /* Should only scan segments which contain grey objects. */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);

  EVENT5(SegScan, seg, SegPool(seg), ss->arena, ss->traces, ss->rank);
  return Method(Seg, seg, scan)(totalReturn, seg, ss);
}


/* SegFix* -- fix a reference to an object in this segment
 *
 * See <design/pool/#req.fix>.
 */

Res SegFix(Seg seg, ScanState ss, Addr *refIO)
{
  AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  return Method(Seg, seg, fix)(seg, ss, refIO);
}

Res SegFixEmergency(Seg seg, ScanState ss, Addr *refIO)
{
  Res res;

  AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  res = Method(Seg, seg, fixEmergency)(seg, ss, refIO);
  AVER_CRITICAL(res == ResOK);
  return res;
}


/* SegReclaim -- reclaim a segment */

void SegReclaim(Seg seg, Trace trace)
{
  AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(Trace, trace);
  AVER_CRITICAL(PoolArena(SegPool(seg)) == trace->arena);

  /* There shouldn't be any grey things left for this trace. */
  AVER_CRITICAL(!TraceSetIsMember(SegGrey(seg), trace));
  /* Should only be reclaiming segments which are still white. */
  AVER_CRITICAL(TraceSetIsMember(SegWhite(seg), trace));

  EVENT4(SegReclaim, trace->arena, SegPool(seg), trace, seg);
  Method(Seg, seg, reclaim)(seg, trace);
}


/* SegWalk -- walk objects in this segment */

void SegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
             void *p, size_t s)
{
  AVERT(Seg, seg);
  AVERT(Format, format);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary values, hence can't be checked. */

  Method(Seg, seg, walk)(seg, format, f, p, s);
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
  /* CHECKL(BoolCheck(seq->queued)); <design/type/#bool.bitfield.check> */

  /* Each tract of the segment must agree about the segment and its
   * pool. Note that even if the CHECKs are compiled away there is
   * still a significant cost in looping over the tracts, hence the
   * guard. See job003778. */
#if defined(AVER_AND_CHECK_ALL)
  {
    Tract tract;
    Addr addr;
    TRACT_TRACT_FOR(tract, addr, arena, seg->firstTract, seg->limit) {
      Seg trseg = NULL; /* suppress compiler warning */

      CHECKD_NOSIG(Tract, tract);
      CHECKL(TRACT_SEG(&trseg, tract));
      CHECKL(trseg == seg);
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


/* segTrivFlip -- ignore trace that's about to flip */

static void segTrivFlip(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(seg->rankSet != RankSetEMPTY);
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


/* segNoBufferFill -- non-method to fill buffer from segment */

static Bool segNoBufferFill(Addr *baseReturn, Addr *limitReturn,
                            Seg seg, Size size, RankSet rankSet)
{
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Seg, seg);
  AVER(size > 0);
  AVERT(RankSet, rankSet);
  NOTREACHED;
  return FALSE;
}


/* segNoBufferEmpty -- non-method to empty buffer to segment */

static void segNoBufferEmpty(Seg seg, Buffer buffer)
{
  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
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
    AVER(segHi == TractSeg(tract));
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
    AVER(seg == TractSeg(tract));
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


/* segNoAccess -- access method for non-GC segs
 *
 * Should be used (for the access method) by segment classes which do
 * not expect to ever have pages which the mutator will fault on. That
 * is, no protected pages, or only pages which are inaccessible by the
 * mutator are protected.
 */
static Res segNoAccess(Seg seg, Arena arena, Addr addr,
                       AccessSet mode, MutatorContext context)
{
  AVERT(Seg, seg);
  AVERT(Arena, arena);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);
  UNUSED(mode);
  UNUSED(context);

  NOTREACHED;
  return ResUNIMPL;
}


/* SegWholeAccess
 *
 * See also SegSingleAccess
 *
 * Should be used (for the access method) by segment classes which
 * intend to handle page faults by scanning the entire segment and
 * lowering the barrier.
 */
Res SegWholeAccess(Seg seg, Arena arena, Addr addr,
                   AccessSet mode, MutatorContext context)
{
  AVERT(Seg, seg);
  AVERT(Arena, arena);
  AVER(arena == PoolArena(SegPool(seg)));
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);

  UNUSED(addr);
  UNUSED(context);
  TraceSegAccess(arena, seg, mode);
  return ResOK;
}


/* SegSingleAccess
 *
 * See also ArenaRead, and SegWhileAccess.
 *
 * Handles page faults by attempting emulation.  If the faulting
 * instruction cannot be emulated then this function returns ResFAIL.
 *
 * Due to the assumptions made below, segment classes should only use
 * this function if all words in an object are tagged or traceable.
 *
 * .single-access.assume.ref: It currently assumes that the address
 * being faulted on contains a plain reference or a tagged
 * non-reference.
 *
 * .single-access.improve.format: Later this will be abstracted
 * through the client object format interface, so that no such
 * assumption is necessary.
 */
Res SegSingleAccess(Seg seg, Arena arena, Addr addr,
                    AccessSet mode, MutatorContext context)
{
  AVERT(Seg, seg);
  AVERT(Arena, arena);
  AVER(arena == PoolArena(SegPool(seg)));
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);

  if (MutatorContextCanStepInstruction(context)) {
    Ref ref;
    Res res;

    ShieldExpose(arena, seg);

    if(mode & SegSM(seg) & AccessREAD) {
      /* Read access. */
      /* .single-access.assume.ref */
      /* .single-access.improve.format */
      ref = *(Ref *)addr;
      /* .tagging: Check that the reference is aligned to a word boundary */
      /* (we assume it is not a reference otherwise). */
      if(WordIsAligned((Word)ref, sizeof(Word))) {
        Rank rank;
        /* See the note in TraceRankForAccess */
        /* (<code/trace.c#scan.conservative>). */

        rank = TraceRankForAccess(arena, seg);
        TraceScanSingleRef(arena->flippedTraces, rank, arena,
                           seg, (Ref *)addr);
      }
    }
    res = MutatorContextStepInstruction(context);
    AVER(res == ResOK);

    /* Update SegSummary according to the possibly changed reference. */
    ref = *(Ref *)addr;
    /* .tagging: ought to check the reference for a tag.  But
     * this is conservative. */
    SegSetSummary(seg, RefSetAdd(arena, SegSummary(seg), ref));

    ShieldCover(arena, seg);

    return ResOK;
  } else {
    /* couldn't single-step instruction */
    return ResFAIL;
  }
}


/* segNoWhiten -- whiten method for non-GC segs */

static Res segNoWhiten(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  NOTREACHED;
  return ResUNIMPL;
}


/* segNoGreyen -- greyen method for non-GC segs */

static void segNoGreyen(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  NOTREACHED;
}


/* segNoBlacken -- blacken method for non-GC segs */

static void segNoBlacken(Seg seg, TraceSet traceSet)
{
  AVERT(Seg, seg);
  AVERT(TraceSet, traceSet);
  NOTREACHED;
}


/* segNoScan -- scan method for non-GC segs */

static Res segNoScan(Bool *totalReturn, Seg seg, ScanState ss)
{
  AVER(totalReturn != NULL);
  AVERT(Seg, seg);
  AVERT(ScanState, ss);
  AVER(PoolArena(SegPool(seg)) == ss->arena);
  NOTREACHED;
  return ResUNIMPL;
}


/* segNoFix -- fix method for non-GC segs */

static Res segNoFix(Seg seg, ScanState ss, Ref *refIO)
{
  AVERT(Seg, seg);
  AVERT(ScanState, ss);
  AVER(refIO != NULL);
  NOTREACHED;
  return ResUNIMPL;
}


/* segNoReclaim -- reclaim method for non-GC segs */

static void segNoReclaim(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);
  NOTREACHED;
}


/* segTrivWalk -- walk method for non-formatted segs */

static void segTrivWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                        void *p, size_t s)
{
  AVERT(Seg, seg);
  AVERT(Format, format);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary, hence can't be checked */
  UNUSED(p);
  UNUSED(s);
  NOOP;
}


/* Class GCSeg -- collectable segment class */

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

  EVENT5(SegSetSummary, PoolArena(SegPool(seg)), seg, SegSize(seg),
         gcseg->summary, RefSetEMPTY);

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
 * Sets the segment greyness to the trace set grey.
 */

static void gcSegSetGrey(Seg seg, TraceSet grey)
{
  AVERT_CRITICAL(Seg, seg);            /* .seg.method.check */
  AVERT_CRITICAL(TraceSet, grey);      /* .seg.method.check */
  AVER_CRITICAL(seg->rankSet != RankSetEMPTY);

  gcSegSetGreyInternal(seg, seg->grey, grey); /* do the work */
}


/* mutatorSegSetGrey -- MutatorSeg method to change greyness of segment
 *
 * As gcSegSetGrey, but also raise or lower the read barrier.
 */

static void mutatorSegSetGrey(Seg seg, TraceSet grey)
{
  TraceSet oldGrey, flippedTraces;
  Arena arena;

  AVERT_CRITICAL(Seg, seg);            /* .seg.method.check */

  oldGrey = seg->grey;

  NextMethod(Seg, MutatorSeg, setGrey)(seg, grey);

  /* The read barrier is raised when the segment is grey for */
  /* some _flipped_ trace, i.e., is grey for a trace for which */
  /* the mutator is black. */
  arena = PoolArena(SegPool(seg));
  flippedTraces = arena->flippedTraces;
  if (TraceSetInter(oldGrey, flippedTraces) == TraceSetEMPTY) {
    if (TraceSetInter(grey, flippedTraces) != TraceSetEMPTY)
      ShieldRaise(arena, seg, AccessREAD);
  } else {
    if (TraceSetInter(grey, flippedTraces) == TraceSetEMPTY)
      ShieldLower(arena, seg, AccessREAD);
  }
}

/* mutatorSegFlip -- update barriers for a trace that's about to flip */

static void mutatorSegFlip(Seg seg, Trace trace)
{
  TraceSet flippedTraces;
  Arena arena;

  NextMethod(Seg, MutatorSeg, flip)(seg, trace);

  arena = PoolArena(SegPool(seg));
  flippedTraces = arena->flippedTraces;
  AVER(!TraceSetIsMember(flippedTraces, trace));

  /* Raise the read barrier if the segment was not grey for any
     currently flipped trace. */
  if (TraceSetInter(SegGrey(seg), flippedTraces) == TraceSetEMPTY) {
    ShieldRaise(arena, seg, AccessREAD);
  } else {
    /* If the segment is grey for some currently flipped trace then
       the read barrier must already have been raised, either in this
       method or in mutatorSegSetGrey. */
    AVER(SegSM(seg) & AccessREAD);
  }
}


/* gcSegSetWhite -- GCSeg method to change whiteness of a segment
 *
 * Sets the segment whiteness to the trace set ts.
 */

static void gcSegSetWhite(Seg seg, TraceSet white)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);            /* .seg.method.check */
  AVERT_CRITICAL(TraceSet, white);     /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  seg->white = BS_BITFIELD(Trace, white);
}


/* gcSegSetRankSet -- GCSeg method to set the rank set of a segment
 *
 * The caller must set the summary to empty before setting the rank
 * set to empty.  The caller must set the rank set to non-empty before
 * setting the summary to non-empty.
 */

static void gcSegSetRankSet(Seg seg, RankSet rankSet)
{
  AVERT_CRITICAL(Seg, seg);                /* .seg.method.check */
  AVERT_CRITICAL(RankSet, rankSet);        /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet)); /* .seg.method.check */

  seg->rankSet = BS_BITFIELD(Rank, rankSet);
}


/* mutatorSegSetRankSet -- MutatorSeg method to set rank set of segment
 *
 * As gcSegSetRankSet, but also sets or clears the write barrier on
 * the segment.
 *
 * If the rank set is made non-empty then the segment's summary is now
 * a subset of the mutator's (which is assumed to be RefSetUNIV) so
 * the write barrier must be imposed on the segment. If the rank set
 * is made empty then there are no longer any references on the
 * segment so the barrier is removed.
 */

static void mutatorSegSetRankSet(Seg seg, RankSet rankSet)
{
  RankSet oldRankSet;

  AVERT_CRITICAL(Seg, seg);                /* .seg.method.check */
  oldRankSet = seg->rankSet;

  NextMethod(Seg, MutatorSeg, setRankSet)(seg, rankSet);

  if (oldRankSet == RankSetEMPTY) {
    if (rankSet != RankSetEMPTY) {
      AVER_CRITICAL(SegGCSeg(seg)->summary == RefSetEMPTY);
      ShieldRaise(PoolArena(SegPool(seg)), seg, AccessWRITE);
    }
  } else {
    if (rankSet == RankSetEMPTY) {
      AVER_CRITICAL(SegGCSeg(seg)->summary == RefSetEMPTY);
      ShieldLower(PoolArena(SegPool(seg)), seg, AccessWRITE);
    }
  }
}


/* mutatorSegSyncWriteBarrier -- raise or lower write barrier on segment
 *
 * We only need to raise the write barrier if the segment contains
 * references, and its summary is strictly smaller than the summary of
 * the unprotectable data (that is, the mutator). We don't maintain
 * such a summary, assuming that the mutator can access all
 * references, so its summary is RefSetUNIV.
 */

static void mutatorSegSyncWriteBarrier(Seg seg)
{
  Arena arena = PoolArena(SegPool(seg));
  /* Can't check seg -- this function enforces invariants tested by SegCheck. */
  if (SegSummary(seg) == RefSetUNIV)
    ShieldLower(arena, seg, AccessWRITE);
  else
    ShieldRaise(arena, seg, AccessWRITE);
}


/* gcSegSetSummary -- GCSeg method to change the summary on a segment */

static void gcSegSetSummary(Seg seg, RefSet summary)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);                 /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  EVENT5(SegSetSummary, PoolArena(SegPool(seg)), seg, SegSize(seg),
         gcseg->summary, summary);

  gcseg->summary = summary;

  AVER_CRITICAL(seg->rankSet != RankSetEMPTY);
}


/* mutatorSegSetSummary -- MutatorSeg method to change summary on segment
 *
 * As gcSegSetSummary, but also raise or lower the write barrier.
 */

static void mutatorSegSetSummary(Seg seg, RefSet summary)
{
  NextMethod(Seg, MutatorSeg, setSummary)(seg, summary);
  mutatorSegSyncWriteBarrier(seg);
}



/* gcSegSetRankSummary -- GCSeg method to set both rank set and summary */

static void gcSegSetRankSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  GCSeg gcseg;

  AVERT_CRITICAL(Seg, seg);                    /* .seg.method.check */
  AVERT_CRITICAL(RankSet, rankSet);            /* .seg.method.check */
  AVER_CRITICAL(rankSet == RankSetEMPTY
                || RankSetIsSingle(rankSet));  /* .seg.method.check */
  gcseg = SegGCSeg(seg);
  AVERT_CRITICAL(GCSeg, gcseg);
  AVER_CRITICAL(&gcseg->segStruct == seg);

  /* rankSet == RankSetEMPTY implies summary == RefSetEMPTY */
  AVER_CRITICAL(rankSet != RankSetEMPTY || summary == RefSetEMPTY);

  seg->rankSet = BS_BITFIELD(Rank, rankSet);
  EVENT5(SegSetSummary, PoolArena(SegPool(seg)), seg, SegSize(seg),
         gcseg->summary, summary);
  gcseg->summary = summary;
}

static void mutatorSegSetRankSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  NextMethod(Seg, MutatorSeg, setRankSummary)(seg, rankSet, summary);
  if (rankSet != RankSetEMPTY)
    mutatorSegSyncWriteBarrier(seg);
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


/* gcSegWhiten -- GCSeg white method */

static Res gcSegWhiten(Seg seg, Trace trace)
{
  AVERT(Seg, seg);
  AVERT(Trace, trace);
  AVER(PoolArena(SegPool(seg)) == trace->arena);

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));

  return ResOK;
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
  CHECKL(FUNCHECK(klass->setSummary));
  CHECKL(FUNCHECK(klass->buffer));
  CHECKL(FUNCHECK(klass->setBuffer));
  CHECKL(FUNCHECK(klass->unsetBuffer));
  CHECKL(FUNCHECK(klass->bufferFill));
  CHECKL(FUNCHECK(klass->bufferEmpty));
  CHECKL(FUNCHECK(klass->setGrey));
  CHECKL(FUNCHECK(klass->setWhite));
  CHECKL(FUNCHECK(klass->setRankSet));
  CHECKL(FUNCHECK(klass->setRankSummary));
  CHECKL(FUNCHECK(klass->merge));
  CHECKL(FUNCHECK(klass->split));
  CHECKL(FUNCHECK(klass->access));
  CHECKL(FUNCHECK(klass->whiten));
  CHECKL(FUNCHECK(klass->greyen));
  CHECKL(FUNCHECK(klass->blacken));
  CHECKL(FUNCHECK(klass->scan));
  CHECKL(FUNCHECK(klass->fix));
  CHECKL(FUNCHECK(klass->fixEmergency));
  CHECKL(FUNCHECK(klass->reclaim));
  CHECKL(FUNCHECK(klass->walk));

  /* Check that segment classes override sets of related methods. */
  CHECKL((klass->init == segAbsInit)
         == (klass->instClassStruct.finish == segAbsFinish));
  CHECKL((klass->init == gcSegInit)
         == (klass->instClassStruct.finish == gcSegFinish));
  CHECKL((klass->merge == segTrivMerge) == (klass->split == segTrivSplit));
  CHECKL((klass->fix == segNoFix) == (klass->fixEmergency == segNoFix));
  CHECKL((klass->fix == segNoFix) == (klass->reclaim == segNoReclaim));

  CHECKS(SegClass, klass);
  return TRUE;
}


/* SegClass -- the vanilla segment class definition */

DEFINE_CLASS(Inst, SegClass, klass)
{
  INHERIT_CLASS(klass, SegClass, InstClass);
  AVERT(InstClass, klass);
}

DEFINE_CLASS(Seg, Seg, klass)
{
  INHERIT_CLASS(&klass->instClassStruct, Seg, Inst);
  klass->instClassStruct.describe = segAbsDescribe;
  klass->instClassStruct.finish = segAbsFinish;
  klass->size = sizeof(SegStruct);
  klass->init = segAbsInit;
  klass->setSummary = segNoSetSummary;
  klass->buffer = segNoBuffer;
  klass->setBuffer = segNoSetBuffer;
  klass->unsetBuffer = segNoUnsetBuffer;
  klass->bufferFill = segNoBufferFill;
  klass->bufferEmpty = segNoBufferEmpty;
  klass->setGrey = segNoSetGrey;
  klass->flip = segTrivFlip;
  klass->setWhite = segNoSetWhite;
  klass->setRankSet = segNoSetRankSet;
  klass->setRankSummary = segNoSetRankSummary;
  klass->merge = segTrivMerge;
  klass->split = segTrivSplit;
  klass->access = segNoAccess;
  klass->whiten = segNoWhiten;
  klass->greyen = segNoGreyen;
  klass->blacken = segNoBlacken;
  klass->scan = segNoScan;
  klass->fix = segNoFix;
  klass->fixEmergency = segNoFix;
  klass->reclaim = segNoReclaim;
  klass->walk = segTrivWalk;
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
  klass->access = SegWholeAccess;
  klass->whiten = gcSegWhiten;
  klass->greyen = gcSegGreyen;
  klass->blacken = gcSegTrivBlacken;
  klass->scan = segNoScan; /* no useful default method */
  klass->fix = segNoFix; /* no useful default method */
  klass->fixEmergency = segNoFix; /* no useful default method */
  klass->reclaim = segNoReclaim; /* no useful default method */
  klass->walk = segTrivWalk;
  AVERT(SegClass, klass);
}


/* MutatorSegClass -- collectable mutator segment class definition */

typedef SegClassStruct MutatorSegClassStruct;

DEFINE_CLASS(Seg, MutatorSeg, klass)
{
  INHERIT_CLASS(klass, MutatorSeg, GCSeg);
  klass->setSummary = mutatorSegSetSummary;
  klass->setGrey = mutatorSegSetGrey;
  klass->flip = mutatorSegFlip;
  klass->setRankSet = mutatorSegSetRankSet;
  klass->setRankSummary = mutatorSegSetRankSummary;
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
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

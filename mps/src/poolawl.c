/* impl.c.poolawl: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $HopeName: MMsrc!poolawl.c(trunk.40) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: design.mps.poolawl
 */


#include "mpm.h"
#include "mpscawl.h"

SRCID(poolawl, "$HopeName: MMsrc!poolawl.c(trunk.40) $");


#define AWLSig	((Sig)0x519b7a37)	/* SIGPooLAWL */

#define AWLGen  ((Serial)1) /* "generation" for AWL pools */


/* Statistics gathering aboput instruction emulation. 
 * In order to support change.dylan.2.0.160044 
 */


/* Per-segment statistics maintained between segment scans*/

typedef struct AWLStatGroupStruct {
  Count refAccesses;   /* accesses involving single reference */
  Count sameAccesses;  /* accesses involving same address as last access */
  Addr lastAccess;     /* the address of last access */
} AWLStatGroupStruct, *AWLStatGroup;

/* Per-pool statistics updated at segment scans */

typedef struct AWLStatTotalStruct {
  Count goodScans;     /* total times a segment scanned at proper rank */
  Count badScans;      /* total times a segment scanned at improper rank */
  Count savedScans;    /* total times an entire segment scan was avoided */
  Count savedAccesses; /* total single references leading to a saved scan */
  Count successive;    /* number of successive single accesses */
  Count declined;      /* number of declined single accesses */
} AWLStatTotalStruct, *AWLStatTotal;


/* design.mps.poolawl.poolstruct */
typedef struct AWLStruct {
  PoolStruct poolStruct;
  Format format;
  Shift alignShift;
  ActionStruct actionStruct;
  double lastCollected;
  Serial gen;               /* associated generation (for SegAlloc) */
  Sig sig;
  AWLStatTotalStruct stats;
} AWLStruct, *AWL;

#define AWLGroupSig ((Sig)0x519a379b)	/* SIGAWLGrouP */

/* design.mps.poolawl.group */
typedef struct AWLGroupStruct {
  Sig sig;
  Seg seg;
  BT mark;
  BT scanned;
  BT alloc;
  Count grains;
  Count free; /* number of free grains */
  AWLStatGroupStruct stats;
} AWLGroupStruct, *AWLGroup;


static Bool AWLCheck(AWL awl);
static Bool AWLGroupCheck(AWLGroup group);


/* PoolPoolAWL -- convert generic Pool to AWL */

#define PoolPoolAWL(pool) \
  PARENT(AWLStruct, poolStruct, (pool))

/* ActionAWL -- converts action to the enclosing AWL */

#define ActionAWL(action)	PARENT(AWLStruct, actionStruct, action)


/* Conversion between indexes and Addrs */
#define awlIndexOfAddr(base, awl, p) \
  (AddrOffset((base), (p)) >> (awl)->alignShift)



/* Management of statistics for monitoring protection-driven accesses */

static void AWLStatGroupInit(AWLGroup group)
{
  AVERT(AWLGroup, group);

  group->stats.refAccesses = 0;
  group->stats.sameAccesses = 0;
  group->stats.lastAccess = NULL;
}


static void AWLStatTotalInit(AWL awl)
{
  AVERT(AWL, awl);

  awl->stats.goodScans = 0;
  awl->stats.badScans = 0;
  awl->stats.savedAccesses = 0;
  awl->stats.savedScans = 0;
  awl->stats.successive = 0;
  awl->stats.declined = 0;
}


/* Single access permission control parameters */

Count AWLSegLimit = 0; /* Number of single accesses permitted per segment */
Bool AWLHaveSegLimit = FALSE;  /* When TRUE, AWLSegLimit applies */

Count AWLTotalLimit = 0; /* Number of single accesses permitted in a row */
Bool AWLHaveTotalLimit = FALSE;  /* When TRUE, AWLTotalLimit applies */


/* Determine whether to permit scanning a single ref */
static Bool AWLStatCanTrySingleAccess(AWL awl, Seg seg, Addr addr)
{
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    AWLGroup group;

    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);
    
    if (AWLHaveTotalLimit) {
      if (AWLTotalLimit < awl->stats.successive) {
	awl->stats.declined++;
	return FALSE; /* decline single access because of total limit */
      }
    }

    if (AWLHaveSegLimit) {
      if (AWLSegLimit < group->stats.refAccesses) {
	awl->stats.declined++;
	return FALSE; /* decline single access because of segment limit */
      }
    }

    return TRUE;

  } else {
    return FALSE; /* Single access is only useful or weak segments */
  }

}



/* Record an access to a segment which required scanning a single ref */
static void AWLStatNoteRefAccess(AWL awl, Seg seg, Addr addr)
{
  AWLGroup group;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  group->stats.refAccesses++; /* increment seg count of ref accesses */
  if (addr == group->stats.lastAccess) {
    /* If this is a repeated access, increment count  */
    group->stats.sameAccesses++;  
  }
  group->stats.lastAccess = addr;
  awl->stats.successive++;  /* Increment count of successive scans */
}


/* Record an access to a segment which required scanning the entire seg */
static void AWLStatNoteSegAccess(AWL awl, Seg seg, Addr addr)
{
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  awl->stats.successive = 0; /* reset count of successive scans */
}


/* Record a scan of a segment which wasn't provoked by an access */
static void AWLStatNoteScan(AWL awl, Seg seg, ScanState ss)
{
  AWLGroup group;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  /* If this is a RankWEAK segment, then record statistics */
  /* about whether weak splatting is being lost */
  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    if (RankWEAK == ss->rank) {
      /* This is "successful" scan at proper rank. */
      awl->stats.goodScans++;
      if (0 < group->stats.refAccesses) {
	/* Accesses have been proceesed singly */
	/* Record that we genuinely did save a protection-provoked scan */
	awl->stats.savedScans++;
	awl->stats.savedAccesses += group->stats.refAccesses;
      }
    } else {
      /* This is "failed" scan at improper rank. */
      awl->stats.badScans++;
    } 
    /* Reinitialize the segment statistics */
    AWLStatGroupInit(group);
  }
}


static void AWLGroupDestroy(AWLGroup group)
{
  AWL awl;
  Pool pool;
  Seg seg;
  Size tableSize;
  Arena arena;
  Count segGrains;

  AVERT(AWLGroup, group);
  seg = group->seg;
  pool = SegPool(seg);
  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* This is one of the few places where it is easy to check */
  /* group->grains, so we do */
  segGrains = SegSize(seg) >> awl->alignShift;
  AVER(segGrains == group->grains);
  tableSize = BTSize(segGrains);
  SegFree(seg);
  ArenaFree(arena, group->alloc, tableSize);
  ArenaFree(arena, group->scanned, tableSize);
  ArenaFree(arena, group->mark, tableSize);
  group->sig = SigInvalid;
  ArenaFree(arena, group, sizeof *group);
}
  
 
static Res AWLGroupCreate(AWLGroup *groupReturn,
                          RankSet rankSet, Pool pool, Size size)
{
  AWL awl;
  Seg seg;
  AWLGroup group;
  void *v;
  Count bits;	/* number of grains */
  Res res;
  Size tableSize;
  Arena arena;
  SegPrefStruct segPrefStruct;

  AVER(groupReturn != NULL);
  AVER(RankSetCheck(rankSet));
  AVERT(Pool, pool);
  AVER(size > 0);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  size = SizeAlignUp(size, ArenaAlign(arena));
  /* beware of large sizes overflowing upon rounding */
  if(size == 0) {
    return ResMEMORY;
  }
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &awl->gen);
  res = SegAlloc(&seg, &segPrefStruct, size, pool);
  if(res != ResOK)
    goto failSegAlloc;
  res = ArenaAlloc(&v, arena, sizeof *group);
  if(res != ResOK)
    goto failArenaAlloc0;
  group = v;
  bits = size >> awl->alignShift;
  tableSize = BTSize(bits);
  res = ArenaAlloc(&v, arena, tableSize);
  if(res != ResOK)
    goto failArenaAllocMark;
  group->mark = v;
  res = ArenaAlloc(&v, arena, tableSize);
  if(res != ResOK)
    goto failArenaAllocScanned;
  group->scanned = v;
  res = ArenaAlloc(&v, arena, tableSize);
  if(res != ResOK)
    goto failArenaAllocAlloc;
  group->alloc = v;
  group->grains = bits;
  BTResRange(group->mark, 0, bits);
  BTResRange(group->scanned, 0, bits);
  BTResRange(group->alloc, 0, bits);
  SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  SegSetP(seg, group);
  group->seg = seg;
  group->free = bits;
  group->sig = AWLGroupSig;
  AWLStatGroupInit(group);
  AVERT(AWLGroup, group);
  *groupReturn = group;
  return ResOK;

failArenaAllocAlloc:
  ArenaFree(arena, group->scanned, tableSize);
failArenaAllocScanned:
  ArenaFree(arena, group->mark, tableSize);
failArenaAllocMark:
  ArenaFree(arena, group, sizeof *group);
failArenaAlloc0:
  SegFree(seg);
failSegAlloc:
  return res;
}


static Bool AWLGroupAlloc(Addr *baseReturn, Addr *limitReturn,
                          AWLGroup group, AWL awl, Size size)
{
  Count n;	/* number of grains equivalent to alloc size */
  Index i, j;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(AWLGroup, group);
  AVERT(AWL, awl);
  AVER(size > 0);
  AVER(size << awl->alignShift >= size);

  if(size > SegSize(group->seg)) {
    return FALSE;
  }
  n = size >> awl->alignShift;
  if(!BTFindLongResRange(&i, &j, group->alloc, 0, group->grains, n)) {
    return FALSE;
  }
  *baseReturn = AddrAdd(SegBase(group->seg), i << awl->alignShift);
  *limitReturn = AddrAdd(SegBase(group->seg), j << awl->alignShift);
  return TRUE;
}


static Res AWLInit(Pool pool, va_list arg)
{
  AWL awl;
  Format format;

  /* weak check, as half way through initialization */
  AVER(pool != NULL);

  awl = PoolPoolAWL(pool);

  format = va_arg(arg, Format);

  AVERT(Format, format);
  awl->format = format;
  awl->alignShift = SizeLog2(pool->alignment);
  ActionInit(&awl->actionStruct, pool);
  awl->lastCollected = ArenaMutatorAllocSize(PoolArena(pool));
  awl->gen = AWLGen;
  awl->sig = AWLSig;
  AWLStatTotalInit(awl);

  AVERT(AWL, awl);

  return ResOK;
}


static void AWLFinish(Pool pool)
{
  AWL awl;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AWLGroup group;

    AVERT(Seg, seg);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);
    AWLGroupDestroy(group);
  }
  ActionFinish(&awl->actionStruct);
}


/* AWLBufferInit -- the buffer init method
 *
 * This just sets rankSet from the client passed parameter.
 */

static Res AWLBufferInit(Pool pool, Buffer buffer, va_list args)
{
  Rank rank;

  AVERT(Pool, pool);
  AVERT(AWL, PoolPoolAWL(pool));

  /* Assumes pun compatibility between Rank and mps_rank_t */
  /* Which is checkd by mpsi_check in impl.c.mpsi */
  rank = va_arg(args, Rank);

  AVER(RankCheck(rank));
  /* AWL only accepts two ranks */
  AVER(rank == RankEXACT || rank == RankWEAK);

  buffer->rankSet = RankSetSingle(rank);
  return ResOK;
}


static Res AWLBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Addr base, limit;
  AWLGroup group;
  AWL awl;
  Res res;
  Ring node, nextNode;

  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg;

    seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);

    /* Only try to allocate in the segment if it is not already */
    /* buffered, and has the same ranks as the buffer. */
    if(SegBuffer(seg) == NULL &&
       SegRankSet(seg) == BufferRankSet(buffer))
      if(group->free << awl->alignShift >= size &&
	 AWLGroupAlloc(&base, &limit, group, awl, size))
	goto found;
  }

  /* No free space in existing groups, so create new group */

  res = AWLGroupCreate(&group, BufferRankSet(buffer), pool, size);
  if(res != ResOK) {
    return res;
  }
  base = SegBase(group->seg);
  limit = SegLimit(group->seg);

found:
  {
    Index i, j;
    i = awlIndexOfAddr(SegBase(group->seg), awl, base);
    j = awlIndexOfAddr(SegBase(group->seg), awl, limit);
    AVER(i < j);
    BTSetRange(group->alloc, i, j);
    /* Objects are allocated black. */
    /* Shouldn't this depend on trace phase?  @@@@ */
    BTSetRange(group->mark, i, j);
    BTSetRange(group->scanned, i, j);
    group->free -= j - i;
  }
  *segReturn = group->seg;
  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


static void AWLBufferEmpty(Pool pool, Buffer buffer)
{
  AWL awl;
  AWLGroup group;
  Addr segBase;
  Index i, j;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(BufferSeg(buffer));
  AVERT(AWLGroup, group);

  segBase = SegBase(BufferSeg(buffer));

  i = awlIndexOfAddr(segBase, awl, BufferGetInit(buffer));
  j = awlIndexOfAddr(segBase, awl, BufferLimit(buffer));
  AVER(i <= j);
  if(i < j) {
    BTResRange(group->alloc, i, j);
    group->free += j - i;
  }
}


static Res AWLWhiten(Pool pool, Trace trace, Seg seg)
{
  /* all parameters checked by generic PoolWhiten */

  /* can only whiten for a single trace, */
  /* see design.mps.poolawl.fun.condemn */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  /* We don't condemn buffered segments so that we avoid */
  /* allocating white objects */
  if(SegBuffer(seg) == NULL) {
    AWL awl;
    AWLGroup group;

    awl = PoolPoolAWL(pool);
    AVERT(AWL, awl);

    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);
    
    BTResRange(group->mark, 0, group->grains);
    BTResRange(group->scanned, 0, group->grains);
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));
  }

  return ResOK;
}

static void AWLGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  if(!TraceSetIsMember(SegWhite(seg), trace->ti)) {
    AWL awl;
    AWLGroup group;

    awl = PoolPoolAWL(pool);
    AVERT(AWL, awl);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);

    SegSetGrey(seg, TraceSetAdd(SegGrey(seg), trace->ti));
    BTSetRange(group->mark, 0, group->grains);
    BTResRange(group->scanned, 0, group->grains);
  }
}

static void AWLBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AWL awl;
  AWLGroup group;

  AVERT(Pool, pool);
  AVER(TraceSetCheck(traceSet));
  AVER(SegCheck(seg));

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);
  
  BTSetRange(group->scanned, 0, group->grains);
}


/* Returns the linked object (or possibly there is none) */
/* see design.mps.poolawl.fun.dependent-object, and */
/* analysis.mps.poolawl.improve.dependent.abstract */
static Bool AWLDependentObject(Addr *objReturn, Addr parent)
{
  Word *object;
  Word *wrapper;
  Word fword;
  Word fl;
  Word ff;

  AVER(objReturn != NULL);
  AVER(parent != (Addr)0);

  object = (Word *)parent;
  wrapper = (Word *)object[0];
  AVER(wrapper != NULL);
  /* check wrapper wrapper is non-NULL */
  AVER(wrapper[0] != 0);
  /* check wrapper wrapper is wrapper wrapper wrapper */
  AVER(wrapper[0] == ((Word *)wrapper[0])[0]);
  fword = wrapper[3];
  ff = fword & 3;
  /* Traceable Fixed part */
  AVER(ff == 1);
  fl = fword & ~3uL;
  /* At least one fixed field */
  AVER(fl >= 1);
  if(object[1] == 0) {
    return FALSE;
  }
  *objReturn = (Addr)object[1];
  return TRUE;
}

static Res awlScanObject(Arena arena, ScanState ss,
                         FormatScanMethod scan, Addr base, Addr limit)
{
  Res res;
  Bool dependent;       /* is there a dependent object? */
  Addr dependentObject; /* base address of dependent object */
  Seg dependentSeg;     /* segment of dependent object */

  AVERT(Arena, arena);
  AVERT(ScanState, ss);
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  if(AWLDependentObject(&dependentObject, base) &&
     SegOfAddr(&dependentSeg, arena, dependentObject)) {
    dependent = TRUE;
  } else {
    dependent = FALSE;
  }

  if(dependent) {
    /* design.mps.poolawl.fun.scan.pass.repeat.object.dependent.expose */
    ShieldExpose(arena, dependentSeg);
    /* design.mps.poolawl.fun.scan.pass.repeat.object.dependent.summary */
    SegSetSummary(dependentSeg, RefSetUNIV);
  }

  res = (*scan)(ss, base, limit);

  if(dependent) {
    ShieldCover(arena, dependentSeg);
  }

  return res;
}

static Res awlScanSinglePass(Bool *anyScannedReturn,
                             ScanState ss, Pool pool,
                             Seg seg, Bool scanAllObjects)
{
  Addr base, limit;
  Addr p;
  Arena arena;
  AWL awl;
  AWLGroup group;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVERT(Bool, scanAllObjects);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  group = SegP(seg);
  AVERT(AWLGroup, group);
  *anyScannedReturn = FALSE;
  base = SegBase(seg);
  limit = SegLimit(seg);
  p = base;
  while(p < limit) {
    Index i;	/* the index into the bit tables corresponding to p */
    Addr objectLimit;
    /* design.mps.poolawl.fun.scan.buffer */
    if(SegBuffer(seg)) {
      Buffer buffer = SegBuffer(seg);

      if(p == BufferScanLimit(buffer) &&
	 BufferScanLimit(buffer) != BufferLimit(buffer)) {
	p = BufferLimit(buffer);
	continue;
      }
    }
    i = awlIndexOfAddr(base, awl, p);
    /* design.mps.poolawl.fun.scan.free */
    if(!BTGet(group->alloc, i)) {
      p = AddrAdd(p, pool->alignment);
      continue;
    }
    /* design.mps.poolawl.fun.scan.object-end */
    objectLimit = (*awl->format->skip)(p);
    /* design.mps.poolawl.fun.scan.scan */
    if(scanAllObjects ||
         (BTGet(group->mark, i) && !BTGet(group->scanned, i))) {
      Res res = awlScanObject(arena, ss, awl->format->scan,
                              p, objectLimit);
      if(res != ResOK) {
	return res;
      }
      *anyScannedReturn = TRUE;
      BTSet(group->scanned, i);
    }
    AVER(p < objectLimit);
    p = AddrAlignUp(objectLimit, pool->alignment);
  }
  AVER(p == limit);

  return ResOK;
}


static Res AWLScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AWL awl;
  AWLGroup group;
  Bool anyScanned;
  Bool scanAllObjects;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);

  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  /* If the scanner isn't going to scan all the objects then the */
  /* summary of the unscanned objects must be added into the scan */
  /* state summary, so that it's a valid summary of the entire */
  /* segment on return. */

  /* This pool assumes disjoint white sets and maintains mark and */
  /* scanned tables (effectively non-white and black tables) with */
  /* respect to the trace with respect to which the segment is */
  /* white.  For any other trace, we cannot tell which objects */
  /* are grey and must therefore scan them all. */

  scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);

  do {
    res = awlScanSinglePass(&anyScanned, ss, pool, seg, scanAllObjects);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  /* we are done if we scanned all the objects or if we did a pass */
  /* and didn't scan any objects (since then, no new object can have */
  /* gotten fixed) */
  } while(!scanAllObjects && anyScanned);

  *totalReturn = scanAllObjects;
  AWLStatNoteScan(awl, seg, ss);
  return ResOK;
}


static Res AWLFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Ref ref;
  Index i;
  AWL awl;
  AWLGroup group;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER(refIO != NULL);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group  = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  ref = *refIO;
  i = awlIndexOfAddr(SegBase(seg), awl, ref);
  
  ss->wasMarked = TRUE;

  switch(ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if(!AddrIsAligned((Addr)ref, pool->alignment) ||
       !BTGet(group->alloc, i)) {
      return ResOK;
    }
  /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    if(!BTGet(group->mark, i)) {
      ss->wasMarked = FALSE;
      if(ss->rank == RankWEAK) {
	*refIO = (Ref)0;
      } else {
	BTSet(group->mark, i);
        SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
      }
    }
    break;
  
  default:
    NOTREACHED;
    return ResUNIMPL;
  }

  return ResOK;
}


static void AWLReclaim(Pool pool, Trace trace, Seg seg)
{
  Addr base;
  AWL awl;
  AWLGroup group;
  Index i;

  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  /* The following line is necessary to avoid a spurious AWL collection */
  /* after an AMC Gen1 collection */
  awl->lastCollected = ArenaMutatorAllocSize(PoolArena(pool));

  base = SegBase(seg);

  i = 0;
  while(i < group->grains) {
    Addr p;
    Index j;
    if(!BTGet(group->alloc, i)) {
      ++i;
      continue;
    }
    p = AddrAdd(base, i << awl->alignShift);
    if(SegBuffer(seg) != NULL) {
      Buffer buffer = SegBuffer(seg);

      if(p == BufferScanLimit(buffer) &&
	 BufferScanLimit(buffer) != BufferLimit(buffer)) {
	i = awlIndexOfAddr(base, awl, BufferLimit(buffer));
	continue;
      }
    }
    j = awlIndexOfAddr(base, awl,
                       AddrAlignUp(awl->format->skip(p), pool->alignment));
    AVER(j <= group->grains);
    if(BTGet(group->mark, i)) {
      AVER(BTGet(group->scanned, i));
      BTSetRange(group->mark, i, j);
      BTSetRange(group->scanned, i, j);
    } else {
      BTResRange(group->mark, i, j);
      BTSetRange(group->scanned, i, j);
      BTResRange(group->alloc, i, j);
      group->free += j - i;
    }
    i = j;
  }
  AVER(i == group->grains);

  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));
}

static Res AWLTraceBegin(Pool pool, Trace trace)
{
  AWL awl;

  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  AVERT(Trace, trace);

  awl->lastCollected = ArenaMutatorAllocSize(PoolArena(pool));
  return ResOK;
}


static Res AWLAccess(Pool pool, Seg seg, Addr addr, 
		     AccessSet mode, MutatorFaultContext context)
{
  AWL awl;
  Res res;

  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVER(SegPool(seg) == pool);

  /* Attempt scanning a single reference if permitted */
  if (AWLStatCanTrySingleAccess(awl, seg, addr)) {
    res = PoolSingleAccess(pool, seg, addr, mode, context);
    switch(res) {
    case ResOK:
      AWLStatNoteRefAccess(awl, seg, addr);
      return ResOK;
    case ResUNIMPL:
      /* Not all accesses can be managed singly. Default to segment */
      break;
    default:
      return res;
    }
  } 

  /* Have to scan the entire seg anyway. */
  res = PoolSegAccess(pool, seg, addr, mode, context);
  if (ResOK == res) {
    AWLStatNoteSegAccess(awl, seg, addr);
  }

  return res;
}


/* defined in impl.c.trace */
extern unsigned long AMCGen0Frequency;
extern unsigned long AMCGen1Frequency;
extern unsigned long AMCGen2Frequency;
extern unsigned long AMCGen2plusFrequencyMultiplier;
extern Serial AMCGenFinal;

static double AWLBenefit(Pool pool, Action action)
{
  AWL awl;
  Arena arena;
  double f;             /* frequency of collection, in Mb of alloc */

  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  AVERT(Action, action);
  AVER(awl == ActionAWL(action));

  switch(awl->gen) {
  case 0: f = AMCGen0Frequency; break;
  case 1: f = AMCGen1Frequency; break;
  case 2: f = AMCGen2Frequency; break;
  default:
    if(awl->gen == AMCGenFinal) {
      f = 1e99; /* Don't ever collect the final generation. */
    } else {
      f = AMCGen2Frequency + AMCGen2plusFrequencyMultiplier * awl->gen;
    }
    break;
  }

  /* When we have both AMC and AWL we want AMC to do the driving of */
  /* the collections, as that collects younger generations too. */
  /* So we make AWLs collections a little less frequent. */
  f += 1;

  arena = PoolArena(pool);

  return (ArenaMutatorAllocSize(arena) - awl->lastCollected) - f * 1024*1024L;
}

static void AWLWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
	            void *p, unsigned long s)
{
  AWL awl;
  AWLGroup group;
  Addr object, base, limit;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  base = SegBase(seg);
  object = base;
  limit = SegLimit(seg);

  while(object < limit) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr next;
    Index i;

    if(SegBuffer(seg) != NULL) {
      Buffer buffer = SegBuffer(seg);
      if(object == BufferScanLimit(buffer) &&
	 BufferScanLimit(buffer) != BufferLimit(buffer)) {
        /* skip over buffered area */
        object = BufferLimit(buffer);
        continue;
      }
      /* since we skip over the buffered area we are always */
      /* either before the buffer, or after it, never in it */
      AVER(object < BufferGetInit(buffer) || BufferLimit(buffer) <= object);
    }
    i = awlIndexOfAddr(base, awl, object);
    if(!BTGet(group->alloc, i)) {
      /* This grain is free */
      object = AddrAdd(object, pool->alignment);
      continue;
    }
    next = AddrAlignUp((*awl->format->skip)(object), pool->alignment);
    if(BTGet(group->mark, i) && BTGet(group->scanned, i)) {
      (*f)(object, awl->format, pool, p, s);
    }
    object = next;
  }
}



struct PoolClassStruct PoolClassAWLStruct = {
  PoolClassSig,
  "AWL",
  sizeof(AWLStruct),
  offsetof(AWLStruct, poolStruct),
  AttrFMT | AttrSCAN | AttrBUF | AttrBUF_RESERVE | AttrGC | AttrINCR_RB,
  AWLInit,
  AWLFinish,
  PoolNoAlloc,
  PoolNoFree,
  AWLBufferInit,
  AWLBufferFill,
  AWLBufferEmpty,
  PoolTrivBufferFinish,
  AWLTraceBegin,
  AWLAccess,
  AWLWhiten,
  AWLGrey,
  AWLBlacken,
  AWLScan,
  AWLFix,
  AWLFix,
  AWLReclaim,
  AWLBenefit,
  PoolCollectAct,
  AWLWalk,
  PoolTrivDescribe,
  PoolClassSig
};


mps_class_t mps_class_awl(void)
{
  return (mps_class_t)&PoolClassAWLStruct;
}


static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKD(Pool, &awl->poolStruct);
  CHECKL(awl->poolStruct.class == &PoolClassAWLStruct);
  CHECKL(1uL << awl->alignShift == awl->poolStruct.alignment);
  CHECKD(Action, &awl->actionStruct);
  CHECKL(ArenaMutatorAllocSize(awl->poolStruct.arena) >= awl->lastCollected);
  /* 30 is just a sanity check really, not a constraint */
  CHECKL(0 <= awl->gen && awl->gen <= 30);
  return TRUE;
}


static Bool AWLGroupCheck(AWLGroup group)
{
  CHECKS(AWLGroup, group);
  CHECKL(SegCheck(group->seg));
  CHECKL(group->mark != NULL);
  CHECKL(group->scanned != NULL);
  CHECKL(group->alloc != NULL);
  /* Can't do any real check on ->grains */
  CHECKL(group->grains > 0);
  CHECKL(group->free <= group->grains);
  return TRUE;
}

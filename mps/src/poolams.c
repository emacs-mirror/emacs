/* impl.c.poolams: AUTOMATIC MARK & SWEEP POOL CLASS
 *
 * $HopeName: MMsrc!poolams.c(trunk.21) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 * 
 * .readership: any MPS developer.
 * 
 * .scope: Implementation of a basic mark/sweep pool.
 *
 * .purpose: A canonical mark/sweep pool to be used as the basis for
 * other mark/sweep pools and for understanding the issues involved in
 * doing mark/sweep collection in the MPS framework.
 * 
 * .design: See design.mps.poolams.
 */

#include "mpm.h"
#include "mpscams.h"

SRCID(poolams, "$HopeName: MMsrc!poolams.c(trunk.21) $");


#define AMSSig          ((Sig)0x519A3599) /* SIGnature AMS */

typedef struct AMSStruct *AMS;
typedef struct AMSStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Format format;                /* format of objects in this pool */
  Shift grainShift;             /* log2 of grain size */
  ActionStruct actionStruct;    /* action of collecting this pool */
  Size size;                    /* total segment size of the pool */
  Size lastReclaimed;           /* size after last reclaim */
  Sig sig;                      /* design.mps.pool.outer-structure.sig */
} AMSStruct;


#define AMSGroupSig     ((Sig)0x519A359B) /* SIGnature AMS GrouP */

typedef struct AMSGroupStruct *AMSGroup;
typedef struct AMSGroupStruct {
  Sig sig;
  Seg seg;                      /* segment of group's memory */
  AMS ams;                      /* owning ams */
  Count grains;                 /* number of grains in this group */
  Count free;                   /* number of free grains in this group */
  BT allocTable;                /* set if grain is allocated */
  /* design.mps.poolams.colour.single */
  Bool marked;                  /* has been marked since last scan */
  BT markTable;                 /* set if grain marked */
  BT scanTable;                 /* set if grain scanned */
} AMSGroupStruct;


/* prototype the check function here; definition is at the end of the
 * file, after the declaration of the pool class structure. */

static Bool AMSCheck(AMS ams);


/* macros to get between child and parent structures */

#define PoolPoolAMS(pool) PARENT(AMSStruct, poolStruct, pool)
#define AMSPool(ams)      (&(ams)->poolStruct)

#define ActionAMS(action) PARENT(AMSStruct, actionStruct, action)
#define AMSAction(ams)    (&(ams)->actionStruct)


/* macros for abstracting index/address computations */
/* design.mps.poolams.addr-index.slow */

#define AMSGroupArena(group)      PoolArena(AMSPool((group)->ams))

/* only use when size is a multiple of the grain size */
#define AMSGrains(ams, size)      ((size) >> (ams)->grainShift)

#define AMSGroupBase(group)       SegBase((group)->seg)

#define AMSGroupLimit(group)      SegLimit((group)->seg)

#define AMSGroupShift(group)      ((group)->ams->grainShift)

#define AMSGroupOffset(group, addr) AddrOffset(AMSGroupBase(group),   \
					       addr)

#define AMSGroupAddr(group, offset) AddrAdd(AMSGroupBase(group), offset)

#define AMSAddrIndex(group, addr) \
  ((Index)(AMSGroupOffset(group, addr) >> AMSGroupShift(group)))

#define AMSIndexAddr(group, index) \
  AMSGroupAddr(group, (index) << AMSGroupShift(group))

#define AMSSegGroup(seg) ((AMSGroup)SegP(seg))


/* The colours of grains.  _MIN and _MAX are here for checking. */

enum {
  AMS_COLOUR_MIN,
  AMS_WHITE,
  AMS_GREY,
  AMS_BLACK,
  AMS_FREE,
  AMS_ILLEGAL,
  AMS_COLOUR_MAX
};

#define AMSColourIsValid(col) (((col) > AMS_COLOUR_MIN) &&           \
                               ((col) < AMS_COLOUR_MAX))


/* AMSGrainColour -- find the colour of a single grain */

static int AMSGrainColour(AMSGroup group, Index index)
{
  Bool mark, scan, alloc;

  /* don't check the group, as AMSGroupCheck calls this */
  AVER(index < group->grains);

  mark = BTGet(group->markTable, index);
  scan = BTGet(group->scanTable, index);
  alloc = BTGet(group->allocTable, index);

  if(mark) {     /* mark */
    if(scan) {   /* mark, scan */
      if(alloc)
        return AMS_BLACK;       /*  mark,  scan,  alloc: black */
      else
        return AMS_ILLEGAL;
    } else {      /* mark, !scan */
      if(alloc)
        return AMS_GREY;        /*  mark, !scan,  alloc: grey */
      else
        return AMS_ILLEGAL;
    } 
  } else {        /* !mark */
    if(scan) {   /* !mark, scan */
      if(alloc)
        return AMS_ILLEGAL;
      else
        return AMS_FREE;        /* !mark,  scan, !alloc: free */
    } else {      /* !mark, !scan */
      if(alloc)
        return AMS_WHITE;       /*  mark, !scan,  alloc: white */
      else
        return AMS_ILLEGAL;
    }
  }
}


/* AMSGroupCheck -- check the group */

static Bool AMSGroupCheck(AMSGroup group)
{
  Index i;

  CHECKS(AMSGroup, group);
  CHECKL(SegCheck(group->seg));
  CHECKU(AMS, group->ams);

  CHECKL(group->grains ==
         (SegSize(group->seg) >> group->ams->grainShift));
  CHECKL(group->grains > 0);

  if(SegWhite(group->seg) != TraceSetEMPTY)
    /* design.mps.poolams.colour.single */
    CHECKL(TraceSetSingle(SegWhite(group->seg)));

  CHECKL(BoolCheck(group->marked));
  CHECKL(group->allocTable != NULL);
  CHECKL(group->markTable != NULL);
  CHECKL(group->scanTable != NULL);

  /* design.mps.poolams.colour.check.slow */
  for (i = 0; i < group->grains; i++) {
    int colour = AMSGrainColour(group, i);
    CHECKL(colour != AMS_ILLEGAL);
    CHECKL(AMSColourIsValid(colour));
  }

  return TRUE;
}


/* AMSGroupCreate -- create a single group */

static Res AMSGroupCreate(AMSGroup *groupReturn, Pool pool, Size size,
                          RankSet rankSet)
{
  AMSGroup group;
  AMS ams;
  Res res;
  Arena arena;
  Seg seg;
  void *p;                      /* for allocating the group header */
  SegPrefStruct segPrefStruct;
  
  AVER(groupReturn != NULL);
  AVERT(Pool, pool);
  AVER(RankSetCheck(rankSet));
  AVER(size > 0);

  ams = PoolPoolAMS(pool);
  AVERT(AMS,ams);
  
  arena = PoolArena(pool);
  
  size = SizeAlignUp(size, ArenaAlign(arena));
  if(size == 0)
    return ResMEMORY; /* overflow in alignment computation */
  
  res = ArenaAlloc(&p, arena, (Size)sizeof(AMSGroupStruct));
  if(res != ResOK)
    goto failGroup;
  group = (AMSGroup)p;

  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  res = SegAlloc(&seg, &segPrefStruct, size, pool);
  if(res != ResOK)
    goto failSeg;
  
  group->seg = seg;
  SegSetP(seg, (void*)group);
  /* see design.mps.seg.field.rankset */
  if(rankSet != RankSetEMPTY) {
    SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  } else {
    SegSetRankAndSummary(seg, rankSet, RefSetEMPTY);
  }

  group->grains = size >> ams->grainShift;
  group->free = group->grains;
  group->marked = FALSE; /* design.mps.poolams.marked.unused */

  res = BTCreate(&group->allocTable, arena, group->grains);
  if(res != ResOK)
    goto failAlloc;
  BTResRange(group->allocTable, 0, group->grains);

  res = BTCreate(&group->markTable, arena, group->grains);
  if(res != ResOK)
    goto failMark;
  BTResRange(group->allocTable, 0, group->grains);

  res = BTCreate(&group->scanTable, arena, group->grains);
  if(res != ResOK)
    goto failScan;
  BTSetRange(group->scanTable, 0, group->grains);

  group->ams = ams;
  group->sig = AMSGroupSig;
  AVERT(AMSGroup, group);
  ams->size += size;

  *groupReturn = group;
  return ResOK;
  
  /* keep the destructions in step with AMSGroupDestroy */
failScan:
  BTDestroy(group->markTable, arena, group->grains);
failMark:
  BTDestroy(group->allocTable, arena, group->grains);
failAlloc:
  SegFree(seg);
failSeg:
  ArenaFree(arena, group, (Size)sizeof(AMSGroupStruct));
failGroup:
  AVER(res != ResOK);
  return res;
}


/* AMSGroupDestroy -- destroy a single group */

static void AMSGroupDestroy(AMSGroup group)
{
  AMS ams;
  Arena arena;

  AVERT(AMSGroup, group);
  ams = group->ams;
  AVERT(AMS, ams);
  arena = PoolArena(AMSPool(ams));
  AVERT(Arena, arena);

  AVER(ams->size >= SegSize(group->seg));

  ams->size -= SegSize(group->seg);

  group->sig = SigInvalid;

  /* keep the destructions in step with AMSGroupCreate failure cases */
  BTDestroy(group->scanTable, arena, group->grains);
  BTDestroy(group->markTable, arena, group->grains);
  BTDestroy(group->allocTable, arena, group->grains);
  SegFree(group->seg);
  ArenaFree(arena, group, (Size)sizeof(AMSGroupStruct));
}  


/* AMSInit -- the pool class initialization method
 * 
 *  Takes one additional argument: the format of the objects
 *  allocated in the pool.  See design.mps.poolams.init.
 */
  
static Res AMSInit(Pool pool, va_list arg)
{
  AMS ams;

  AVERT(Pool, pool);

  ams = PoolPoolAMS(pool);

  ams->format = va_arg(arg, Format);
  AVERT(Format, ams->format);
  pool->alignment = ams->format->alignment;
  ams->grainShift = SizeLog2(PoolAlignment(pool));

  ActionInit(AMSAction(ams), pool);

  ams->size = 0;
  ams->lastReclaimed = 0;

  ams->sig = AMSSig;
  AVERT(AMS, ams);

  return ResOK;
}


/* AMSFinish -- the pool class finishing method
 * 
 * Destroys all the groups in the pool. Can't invalidate the AMS until
 * we've destroyed all the groups, as it may be checked.
 */

static void AMSFinish(Pool pool)
{
  AMS ams;
  Ring ring, node;              /* for iterating over the segments */

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  ring = PoolSegRing(pool);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Seg seg = SegOfPoolRing(node);
    AMSGroup group = (AMSGroup)SegP(seg);

    AVER(group->ams == ams);

    AMSGroupDestroy(group);

    node = next;
  }

  ActionFinish(AMSAction(ams));
  /* can't invalidate the AMS until we've destroyed all the groups */
  ams->sig = SigInvalid;
}


/* AMSGroupAlloc -- try to allocate an area in the given group
 * 
 * Tries to find an area of at least the given size.  If successful,
 * makes that area black and returns its base and limit grain indices.
 */

static Bool AMSGroupAlloc(Index *baseReturn, Index *limitReturn,
                          AMSGroup group, Size size)
{
  AMS ams;
  Size grains;
  Bool b;                       /* can we allocate in this group? */
  Index base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  /* group has already been checked, in AMSBufferFill. */

  ams = group->ams;
  AVERT(AMS, ams);

  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(AMSPool(ams))));

  grains = AMSGrains(ams, size);
  AVER(grains > 0);
  if(grains > group->grains)
    return FALSE;

  b = BTFindLongResRange(&base, &limit, group->allocTable,
                         0, group->grains, grains);
  if(!b)
    return FALSE;

  /* design.mps.poolams.colour.bits */
  AVER(BTIsResRange(group->markTable, base, limit));
  AVER(BTIsSetRange(group->scanTable, base, limit));
  AVER(BTIsResRange(group->allocTable, base, limit));

  BTSetRange(group->allocTable, base, limit);
  BTSetRange(group->markTable, base, limit);
  group->free -= limit - base;

  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


/* AMSBufferInit -- the buffer init method
 *
 * This just sets rankSet.  See design.mps.poolams.buffer-init.
 */

static Res AMSBufferInit(Pool pool, Buffer buffer, va_list args)
{
  Rank rank = va_arg(args, Rank);

  AVERT(Pool, pool);
  AVERT(AMS, PoolPoolAMS(pool));
  AVERT(Rank, rank);

  buffer->rankSet = RankSetSingle(rank);
  return ResOK;
}


/* AMSBufferFill -- the pool class buffer fill method
 * 
 * Iterates over the segments looking for space.  See
 * design.mps.poolams.fill
 */

static Res AMSBufferFill(Seg *segReturn,
                         Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Res res;
  AMS ams;
  AMSGroup group;
  Ring node, ring, nextNode;    /* for iterating over the segments */
  Index base, limit;
  RankSet rankSet;
  Bool b;                       /* the return value of AMSGroupAlloc */

  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  rankSet = BufferRankSet(buffer);

  /* design.mps.poolams.fill.slow */
  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    if(SegBuffer(seg) == NULL && SegRankSet(seg) == rankSet) {
      group = AMSSegGroup(seg);
      AVERT_CRITICAL(AMSGroup, group);
      if(group->free > AMSGrains(ams, size)) {
        b = AMSGroupAlloc(&base, &limit, group, size);
        if(b)
          goto found;
      }
    }
  }

  /* no group has enough room; make a new group */
  res = AMSGroupCreate(&group, pool, size, rankSet);
  if(res != ResOK)
    return res;
  b = AMSGroupAlloc(&base, &limit, group, size);
  
found:
  AVER(b);
  *segReturn = group->seg;
  *baseReturn = AMSIndexAddr(group, base);
  *limitReturn = AMSIndexAddr(group, limit);
  return ResOK;
}


/* AMSBufferEmpty -- the pool class buffer empty method
 * 
 * Checks that the buffer is still black, and frees the unused part of
 * the buffer.
 */

static void AMSBufferEmpty(Pool pool, Buffer buffer)
{
  AMS ams;
  Addr init, limit;
  Index initIndex, limitIndex;
  Seg seg;
  AMSGroup group;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer,buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));
  
  seg = BufferSeg(buffer);
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->seg == seg);
  
  init = BufferGetInit(buffer);
  limit = BufferLimit(buffer);

  AVER(AddrIsAligned(init, PoolAlignment(pool)));
  AVER(AddrIsAligned(limit, PoolAlignment(pool)));

  if(init == limit)
    return;

  initIndex = AMSAddrIndex(group, init);
  limitIndex = AMSAddrIndex(group, limit);

  /* design.mps.poolams.colour.bits */
  AVER(BTIsSetRange(group->markTable, initIndex, limitIndex));
  AVER(BTIsSetRange(group->scanTable, initIndex, limitIndex));
  AVER(BTIsSetRange(group->allocTable, initIndex, limitIndex));

  BTResRange(group->markTable, initIndex, limitIndex);
  BTResRange(group->allocTable, initIndex, limitIndex);
  group->free += limitIndex - initIndex;
}


/* AMSRangeWhiten -- whitens a part of a group
 * 
 * Split out of AMSWhiten because it's used in more than one place.
 */

static void AMSRangeWhiten(AMSGroup group, Index base, Index limit)
{
  if(base != limit) {
    AVER(base < limit);
    AVER(limit <= group->grains);
    
    /* either black or free */
    AVER(BTIsSetRange(group->scanTable, base, limit));
    AVER(BTRangesSame(group->allocTable, group->markTable,
		      base, limit));
    
    /* black -> white, free -> free */
    BTResRange(group->markTable, base, limit);
    BTCopyInvertRange(group->allocTable, group->scanTable, base, limit);
  }
}


/* AMSWhiten -- the pool class segment whitening method */

static Res AMSWhiten(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSGroup group;
  Buffer buffer;                /* the seg's buffer, if it has one */

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Trace, trace);
  AVER(SegCheck(seg));

  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->seg == seg);
  AVER(group->ams == ams);

  /* design.mps.poolams.colour.single */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  buffer = SegBuffer(seg);
  if(buffer != NULL) {
    /* Whiten everything except the buffer. @@@@ */
    Index scanLimitIndex, limitIndex;
    scanLimitIndex = AMSAddrIndex(group, BufferScanLimit(buffer));
    limitIndex = AMSAddrIndex(group, BufferLimit(buffer));
    
    AMSRangeWhiten(group, 0, scanLimitIndex);
    AMSRangeWhiten(group, limitIndex, group->grains);
  } else { /* whiten whole seg */
    AMSRangeWhiten(group, 0, group->grains);
  }

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));

  return ResOK;
}


/* AMSObjectFunction is the type of the function which AMSIterate
 * applies to each object in a group. */

typedef Res (*AMSObjectFunction)(
              AMSGroup group, /* the group */
              Index i,        /* the object grain index */
              Addr p,         /* the object address */
              Addr next,      /* the address after the object */
              int colour,     /* the object colour */
              void *closure); /* the iteration closure */

#define AMSObjectFunctionCheck(f) \
  ((f) != NULL) /* that's the best we can do */


/* AMSIterate -- applies a function to each object in a group
 * 
 * AMSIterate(ams, group, seg, arena, f, closure) applies f to all the
 * objects in the group.  It skips the buffer, if any (from
 * BufferScanLimit to BufferLimit).
 */

static Res AMSIterate(AMS ams, AMSGroup group, Seg seg, Arena arena,
		      AMSObjectFunction f, void *closure)
{
  Res res;
  Format format;
  Align alignment;
  Addr p;
  Addr limit;

  AVERT(AMS, ams);
  AVERT(AMSGroup, group);
  AVERT(Seg, seg);
  AVERT(Arena, arena);
  AVERT(AMSObjectFunction, f);
  /* Can't check closure */

  format = ams->format;
  alignment = PoolAlignment(AMSPool(ams));
  UNUSED(arena);

  p = SegBase(seg);
  limit = SegLimit(seg);

  while (p < limit) { /* loop over the objects in the group */
    Index i;
    Addr next;
    int colour;
    Buffer buffer = SegBuffer(seg);

  
    if(buffer != NULL
       && p == BufferScanLimit(buffer) && p != BufferLimit(buffer)) {
      /* skip buffer */
      next = BufferLimit(buffer); 
      AVER(AddrIsAligned(next, alignment));
    } else {
      AVER((buffer == NULL) ||
	   (p < BufferScanLimit(buffer)) ||
	   (p >= BufferLimit(buffer)));  /* not in the buffer */

      i = AMSAddrIndex(group, p);
      colour = AMSGrainColour(group, i);
      AVER(AMSColourIsValid(colour));
      AVER(colour != AMS_ILLEGAL);
      if(colour == AMS_FREE) { /* no object here */
        next = AddrAdd(p, alignment);
      } else { /* there is an object here */
        next = (*format->skip)(p);
        AVER(AddrIsAligned(next, alignment));

	/* apply the object function */
	res = (*f)(group, i, p, next, colour, closure);
	if(res != ResOK)
	  return res;
      }
    }
    AVER(next > p); /* make sure we make progress */
    p = next;
  }
  AVER(p == limit);
  return ResOK;
}


/* AMSBlackenObject -- blacken a single object (if it is grey)
 * 
 * This is the object function passed to AMSIterate by AMSBlacken.
 * It just blackens the object if it is grey.  It takes no closure.
 */

static Res AMSBlackenObject(AMSGroup group,
			    Index i, Addr p, Addr next, int colour,
			    void *clos)
{
  /* group has already been checked, in AMSIterate. */
  AVER(i < group->grains);
  AVER(p != 0);
  AVER(p < next);
  AVER(clos == NULL);
  AVER(colour != AMS_ILLEGAL);
  AVER(AMSColourIsValid(colour));

  /* if the object is grey, make it black */
  if(colour == AMS_GREY) 
    BTSet(group->scanTable, i);

  return ResOK;
}


/* AMSBlacken -- the pool class segment blackening method */

static void AMSBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  Res res;
  AMS ams;
  Arena arena;
  AMSGroup group;

  AVERT(Pool, pool);
  AVER(TraceSetCheck(traceSet));
  AVER(SegCheck(seg));

  /* Only do anything if the bitmaps apply to one of these traces, */
  /* see design.mps.poolams.colour.determine. */
  if(TraceSetInter(SegWhite(seg), traceSet) != TraceSetEMPTY) {
    ams = PoolPoolAMS(pool);
    AVERT(AMS, ams);
    arena = PoolArena(pool);

    group = AMSSegGroup(seg);
    AVERT(AMSGroup, group);

    ShieldExpose(arena, seg); /* so we can skip through it */
    res = AMSIterate(ams, group, seg, arena, AMSBlackenObject, NULL);
    AVER(res == ResOK); /* AMSBlackenObject always returns ResOK */
    ShieldCover(arena, seg);

    group->marked = FALSE; /* nothing grey anymore */
  }
  /* @@@@ What about SegGrey? */
}

/* The closure of the object scanning function */

struct AMSScanClosureStruct {
  ScanState ss;
  Bool scanAllObjects; /* scan non-grey objects? */
};

typedef struct AMSScanClosureStruct *AMSScanClosure;


/* AMSScanObject -- scan a single object
 * 
 * This is the object function passed to AMSIterate by AMSScan.
 */

static Res AMSScanObject(AMSGroup group,
			 Index i, Addr p, Addr next, int colour,
			 void *clos)
{
  Format format;
  AMSScanClosure closure;

  /* group has already been checked, in AMSIterate. */
  AVER(i < group->grains);
  AVER(p != 0);
  AVER(p < next);

  /* check the closure */
  AVER(clos != NULL);
  closure = clos;
  AVERT(ScanState, closure->ss);
  AVER(BoolCheck(closure->scanAllObjects));

  AVER(colour != AMS_ILLEGAL);
  AVER(AMSColourIsValid(colour));

  format = group->ams->format;
  AVERT(Format, format);

  /* The design incorrectly assumes scanAllObjects => everything is */
  /* grey, so we have to scan everything if scanAllObjects is true. */
  if(colour == AMS_GREY || closure->scanAllObjects) {
    Res res = (*format->scan)(closure->ss, p, next);
    if(res != ResOK)
      return res;
  }

  if(colour == AMS_GREY) /* blacken the object */
    BTSet(group->scanTable, i);

  return ResOK;
}


/* AMSScan -- the pool class segment scanning method
 *
 * See design.mps.poolams.scan
 */

static Res AMSScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Res res;
  AMS ams;
  Arena arena;
  AMSGroup group;
  struct AMSScanClosureStruct closureStruct;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  arena = PoolArena(pool);

  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);

  /* Check that we're not in the grey mutator phase (see */
  /* design.mps.poolams.not-req.grey). */
  AVER(TraceSetSub(ss->traces, arena->flippedTraces));

  closureStruct.scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);
  closureStruct.ss = ss;

  if(closureStruct.scanAllObjects) {

    res = AMSIterate(ams, group, seg, arena, AMSScanObject,
		     &closureStruct);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    group->marked = FALSE;
    
  } else { /* design.mps.poolams.scan.iter */

    AVER(group->marked); /* we're only scanning if something is grey */
    do { /* design.mps.poolams.marked.scan */
      group->marked = FALSE; 
      res = AMSIterate(ams, group, seg, arena, AMSScanObject,
		       &closureStruct);
      if(res != ResOK) {
        group->marked = TRUE; /* design.mps.poolams.marked.scan.fail */
	*totalReturn = FALSE;
        return res;
      }
    } while(group->marked);

  }

  *totalReturn = closureStruct.scanAllObjects;
  return ResOK;
}


/* AMSFix -- the pool class fixing method */

static Res AMSFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AMS ams;
  AMSGroup group;
  Arena arena;
  Index i;                      /* the index of the fixed grain */
  Ref ref;
  int colour;                   /* the colour of the fixed grain */

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(ScanState, ss);
  AVER(SegCheck(seg));
  AVER(refIO != NULL);
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  
  arena = PoolArena(pool);

  /* @@@@ We should check that we're not in the grey mutator phase */
  /* (see design.mps.poolams.not-req.grey), but there's no way of */
  /* doing that here (this can be called from RootScan, during flip). */

  ref = *refIO;
  i = AMSAddrIndex(group, ref);
  colour = AMSGrainColour(group, i);
  AVER(colour != AMS_ILLEGAL);
  AVER(AMSColourIsValid(colour));
  
  ss->wasMarked = TRUE;

  switch (ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if(!AddrIsAligned((Addr)ref, PoolAlignment(pool)) ||
       (colour == AMS_FREE)) {
      break;
    }
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    AVER(AddrIsAligned((Addr)ref, PoolAlignment(pool)));
    AVER(colour != AMS_FREE);
    if(colour == AMS_WHITE) {
      ss->wasMarked = FALSE;
      if(ss->rank == RankWEAK) { /* then splat the reference */
        *refIO = (Ref)0;
      } else {
	/* turn this object grey */
        BTSet(group->markTable, i);
	/* turn this segment grey */
	SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
	/* mark it for scanning - design.mps.poolams.marked.fix */
	group->marked = TRUE;
      }
    }
    break;
  default:
    NOTREACHED;
  }

  return ResOK;
}


/* AMSReclaimObject -- inspect a single object for reclamation
 * 
 * This is the object function passed to AMSIterate by AMSReclaim.
 */

static Res AMSReclaimObject(AMSGroup group,
			    Index i, Addr p, Addr next, int colour,
			    void *clos)
{
  Bool *anySurvivorsP;      /* update to true if the object survives */
  Index j;                  /* one more than the last grain index */

  /* group has already been checked, in AMSIterate. */
  AVER(i < group->grains);
  AVER(p != 0);
  AVER(p < next);

  /* check closure */
  AVER(clos != NULL);
  anySurvivorsP = clos;
  AVER(BoolCheck(*anySurvivorsP));

  AVER(AMSColourIsValid(colour));
  AVER(colour != AMS_ILLEGAL);
  AVER(colour != AMS_GREY); /* no grey objects now */
  j = AMSAddrIndex(group, next);
  
  if(colour == AMS_WHITE) { /* then we can free it */
    BTResRange(group->markTable, i, j);
    BTSetRange(group->scanTable, i, j);
    BTResRange(group->allocTable, i, j);
    group->free += j - i;
  } else { /* the object survived collection */
    AVER(colour == AMS_BLACK);
    /* make it all black */
    BTSetRange(group->markTable, i, j);
    BTSetRange(group->scanTable, i, j);
    *anySurvivorsP = TRUE;
  }
  return ResOK;
}


/* AMSReclaim -- the pool class reclamation method */

static void AMSReclaim(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSGroup group;
  Arena arena;
  Res res;
  Count oldFree;
  Bool anySurvivors;            /* did anything survive? */

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Seg, seg);
  arena = PoolArena(pool);

  group = AMSSegGroup(seg);

  AVER(group->marked == FALSE); /* there must be nothing grey */

  anySurvivors = FALSE;
  oldFree = group->free;

  res = AMSIterate(ams, group, seg, arena, AMSReclaimObject,
		   &anySurvivors);
  AVER(res == ResOK); /* AMSReclaimObject always returns ResOK */

  trace->reclaimSize += (group->free - oldFree) << ams->grainShift;

  if((SegBuffer(seg) == NULL) && !anySurvivors) {
    /* No survivors */
    AMSGroupDestroy(group);
    /* design.mps.poolams.benefit.guess */
    ams->lastReclaimed = ams->size;
  } else
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));
}


/* AMSBenefit -- the pool class benefit computation method
 *
 * This does not compute a real benefit, but something which works
 * well enough to run tests.  See design.mps.poolams.benefit.guess.
 */

int AMSRatioDenominator = 1;
int AMSRatioNumerator = 2;
Size AMSMinimumCollectableSize = 128*(Size)1024;

static double AMSBenefit(Pool pool, Action action)
{
  AMS ams;
  double benefit;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Action, action);
  AVER(ams == ActionAMS(action));

  if((ams->size > AMSMinimumCollectableSize)
      && (ams->size * AMSRatioNumerator
          > ams->lastReclaimed * AMSRatioDenominator)) {
    /* design.mps.poolams.benefit.repeat */
    ams->lastReclaimed = ams->size; 
    benefit = 1.0;
  } else {
    benefit = 0.0;
  }
  return benefit;
}


/* AMSSegDescribe -- describe an AMS segment */

static Res AMSSegDescribe(AMS ams, Seg seg, mps_lib_FILE *stream)
{
  Res res;
  AMSGroup group;
  Buffer buffer;               /* the segment's buffer, if it has one */
  Index i;

  AVERT(AMS, ams);
  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->ams == ams);
  /* can't check stream, as there are no conditions on mps_lib_FILE * */

  buffer = SegBuffer(seg);

  if(buffer != NULL)
    AVERT(Buffer, buffer);

  res = WriteF(stream,
               "AMS Group $P {\n", (WriteFP)group,
               "  seg $P [$A,$A)\n",
               (WriteFP)seg, AMSGroupBase(group), AMSGroupLimit(group),
               "  AMS $P\n", (WriteFP)ams,
               "  grains $W\n", (WriteFW)group->grains,
               "  tables: alloc $P, mark $P, scan $P\n",
                 (WriteFP)group->allocTable,
                 (WriteFP)group->markTable,
                 (WriteFP)group->scanTable,
               "  map: \n",
               NULL);
               
  for (i=0 ; i < group->grains; ++ i) {
    int colour;
    char c = 0;
    if(i % 64 == 0) {
      res = WriteF(stream, "\n  ", NULL);
      if(res != ResOK)
        return res;
    }
    if(buffer != NULL) {
      Index baseIndex = AMSAddrIndex(group, BufferBase(buffer));
      Index limitIndex = AMSAddrIndex(group, BufferLimit(buffer));
      Index scanLimIndex = AMSAddrIndex(group, BufferScanLimit(buffer));
      Index initIndex = AMSAddrIndex(group, BufferGetInit(buffer));
      Index allocIndex = AMSAddrIndex(group, BufferAlloc(buffer));

      if(i == limitIndex)
        c = ']';
      else if(i == baseIndex)
        c = '[';
      else if(i == scanLimIndex)
        c = '<';
      else if(i == initIndex)
        c = '|';
      else if(i == allocIndex)
        c = '>';

      if(c != 0) {
        res = WriteF(stream, "$C", c, NULL);
        if(res != ResOK)
          return res;
      }
    }

    colour = AMSGrainColour(group, i);
    AVER(AMSColourIsValid(colour));
    switch(colour) {
    case AMS_FREE:
      c = '.';
      break;
    case AMS_WHITE:
      c = '-';
      break;
    case AMS_GREY:
      c = '+';
      break;
    case AMS_BLACK:
      c = '*';
      break;
    case AMS_ILLEGAL: /* maybe we're in the middle of debugging */
      c = '!';
      break;
    default:
      NOTREACHED;
    }
    res = WriteF(stream, "$C", c, NULL);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream, "\n} AMS Group $P\n", (WriteFP)group);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* AMSDescribe -- the pool class description method
 * 
 * Iterates over the segments, describing all of them.
 */

static Res AMSDescribe(Pool pool, mps_lib_FILE *stream)
{
  AMS ams;
  Ring node, nextNode;
  Res res;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  /* can't check stream, as there are no conditions on mps_lib_FILE * */

  res = WriteF(stream,
               "AMS $P {\n", (WriteFP)ams,
               "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               "  size $W, lastReclaimed $W\n",
               (WriteFW)ams->size, (WriteFW)ams->lastReclaimed,
               "  format $P ($U)\n",
               (WriteFP)ams->format, (WriteFU)ams->format->serial,
               "  grain shift $U\n", (WriteFU)ams->grainShift,
               NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream,
               "  segments [* = black, + = grey, - = white, . = free,\n"
               "            ! = bad,\n"
	       "            [ = buffer base\n"
	       "            < = buffer scan limit\n"
	       "            | = buffer init\n"
	       "            > = buffer alloc\n"
	       "            ] = buffer limit\n",
               NULL);
  if(res != ResOK)
    return res;

  RING_FOR(node, PoolSegRing(pool), nextNode) {
    Seg seg = SegOfPoolRing(node);
    AMSSegDescribe(ams, seg, stream);
  }

  res = WriteF(stream, "} AMS $P\n",(WriteFP)ams, NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* PoolClassAMSStruct -- the pool class descriptor */

static PoolClassStruct PoolClassAMSStruct = {
  PoolClassSig,
  "AMS",                     /* name */
  sizeof(AMSStruct),         /* size */
  offsetof(AMSStruct, poolStruct),      /* offset */
  AttrFMT | AttrSCAN | AttrBUF | AttrBUF_RESERVE | AttrGC | AttrINCR_RB,
  AMSInit,                   /* init */
  AMSFinish,                 /* finish */
  PoolNoAlloc,               /* design.mps.poolams.no-alloc */
  PoolNoFree,                /* design.mps.poolams.no-free */
  AMSBufferInit,
  AMSBufferFill,             /* bufferFill */
  AMSBufferEmpty,            /* bufferEmpty */
  PoolTrivBufferFinish,
  PoolTrivTraceBegin,
  PoolSegAccess,
  AMSWhiten,                 /* whiten */
  PoolTrivGrey,              /* design.mps.poolams.colour.determine */
  AMSBlacken,                /* blacken */
  AMSScan,                   /* scan */
  AMSFix,                    /* fix */
  AMSFix,                    /* emergency fix */
  AMSReclaim,                /* reclaim */
  AMSBenefit,                /* benefit */
  PoolCollectAct,            /* act */
  PoolNoWalk,                /* implement this @@@@ */
  AMSDescribe,               /* describe */
  PoolClassSig               /* impl.h.mpm.class.end-sig */
};


/* AMSCheck -- the check method for an AMS */

static Bool AMSCheck(AMS ams)
{
  CHECKS(AMS, ams);
  CHECKD(Pool, AMSPool(ams));
  CHECKL(AMSPool(ams)->class == &PoolClassAMSStruct);
  CHECKD(Format, ams->format);
  CHECKL((PoolAlignment(AMSPool(ams)) >> ams->grainShift) == 1);
  CHECKL(PoolAlignment(AMSPool(ams)) == ams->format->alignment);
  CHECKD(Action, AMSAction(ams));
  CHECKL(AMSAction(ams)->pool == AMSPool(ams));
  CHECKL(SizeIsAligned(ams->size, PoolAlignment(AMSPool(ams))));
  CHECKL(SizeIsAligned(ams->lastReclaimed,
		       PoolAlignment(AMSPool(ams))));

  return TRUE;
}


/* mps_class_ams -- return the pool class descriptor to the client */

mps_class_t mps_class_ams(void)
{
  return (mps_class_t)&PoolClassAMSStruct;
}

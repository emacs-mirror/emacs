/* impl.c.poolams: AUTOMATIC MARK & SWEEP POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See design.mps.poolams.
 *
 *
 * TRANSGRESSSIONS
 *
 * .no-check.local: We have decided to omit checks in local functions of
 * structure arguments that are simply passed down through the caller
 * (as opposed to being constructed by the caller).
 */

#include "mpscams.h"
#include "poolams.h"
#include "mpm.h"
#include <stdarg.h>

SRCID(poolams, "$Id$");


#define AMSSig          ((Sig)0x519A3599) /* SIGnature AMS */
#define AMSSegSig       ((Sig)0x519A3559) /* SIGnature AMS SeG */


/* AMSSegCheck -- check an AMS segment */

Bool AMSSegCheck(AMSSeg amsseg)
{
  Seg seg = AMSSeg2Seg(amsseg);
  CHECKS(AMSSeg, amsseg);
  CHECKL(GCSegCheck(&amsseg->gcSegStruct));
  CHECKU(AMS, amsseg->ams);
  CHECKL(AMS2Pool(amsseg->ams) == SegPool(seg));
  CHECKL(RingCheck(&amsseg->segRing));

  CHECKL(amsseg->grains == AMSGrains(amsseg->ams, SegSize(seg)));
  CHECKL(amsseg->grains > 0);
  CHECKL(amsseg->grains >= amsseg->free + amsseg->newAlloc);

  if (SegWhite(seg) != TraceSetEMPTY)
    /* design.mps.poolams.colour.single */
    CHECKL(TraceSetIsSingle(SegWhite(seg)));

  CHECKL(BoolCheck(amsseg->marksChanged));
  CHECKL(amsseg->allocTable != NULL);
  CHECKL(amsseg->nongreyTable != NULL);
  CHECKL(amsseg->nonwhiteTable != NULL);

  return TRUE;
}


/* amsCreateTables -- create the tables for an AMS seg */

static Res amsCreateTables(BT *allocReturn,
                           BT *nongreyReturn, BT *nonwhiteReturn,
                           Arena arena, Count length)
{
  Res res;
  BT allocTable, nongreyTable, nonwhiteTable;

  AVER(allocReturn != NULL);
  AVER(nongreyReturn != NULL);
  AVER(nonwhiteReturn != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);

  res = BTCreate(&allocTable, arena, length);
  if (res != ResOK)
    goto failAlloc;
  res = BTCreate(&nongreyTable, arena, length);
  if (res != ResOK)
    goto failGrey;
  res = BTCreate(&nonwhiteTable, arena, length);
  if (res != ResOK)
    goto failWhite;

  *allocReturn = allocTable;
  *nongreyReturn = nongreyTable;
  *nonwhiteReturn = nonwhiteTable;
  return ResOK;

failWhite:
  BTDestroy(nongreyTable, arena, length);
failGrey:
  BTDestroy(allocTable, arena, length);
failAlloc:
  return res;
}


/* amsDestroyTables -- destroy the tables for an AMS seg */

static void amsDestroyTables(BT allocTable,
                             BT nongreyTable, BT nonwhiteTable,
                             Arena arena, Count length)
{
  AVER(allocTable != NULL);
  AVER(nongreyTable != NULL);
  AVER(nonwhiteTable != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);

  BTDestroy(nonwhiteTable, arena, length);
  BTDestroy(nongreyTable, arena, length);
  BTDestroy(allocTable, arena, length);
}


/* AMSSegInit -- Init method for AMS segments */

static Res AMSSegInit(Seg seg, Pool pool, Addr base, Size size,
                      Bool reservoirPermit, va_list args)
{
  SegClass super;
  AMSSeg amsseg;
  Res res;
  Arena arena;
  AMS ams;

  AVERT(Seg, seg);
  amsseg = Seg2AMSSeg(seg);
  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  arena = PoolArena(pool);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(AMSSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    goto failNextMethod;

  amsseg->grains = size >> ams->grainShift;
  amsseg->free = amsseg->grains;
  amsseg->newAlloc = (Count)0;
  amsseg->marksChanged = FALSE; /* design.mps.poolams.marked.unused */
  amsseg->ambiguousFixes = FALSE;

  res = amsCreateTables(&amsseg->allocTable,
                        &amsseg->nongreyTable, &amsseg->nonwhiteTable,
                        arena, amsseg->grains);
  if (res != ResOK)
    goto failCreateTables;

  /* start off using firstFree, see design.mps.poolams.no-bit */
  amsseg->allocTableInUse = FALSE;
  amsseg->firstFree = 0;
  amsseg->colourTablesInUse = FALSE;

  amsseg->ams = ams;
  RingInit(&amsseg->segRing);
  RingAppend((ams->allocRing)(ams, SegRankSet(seg), size),
             &amsseg->segRing);

  amsseg->sig = AMSSegSig;
  ams->size += size;
  AVERT(AMSSeg, amsseg);

  return ResOK;

failCreateTables:
  super->finish(seg);
failNextMethod:
  return res;
}


/* AMSSegFinish -- Finish method for AMS segments */

static void AMSSegFinish(Seg seg)
{
  SegClass super;
  AMSSeg amsseg;
  AMS ams;
  Arena arena;

  AVERT(Seg, seg);
  amsseg = Seg2AMSSeg(seg);
  AVERT(AMSSeg, amsseg);
  ams = amsseg->ams;
  AVERT(AMS, ams);
  arena = PoolArena(AMS2Pool(ams));
  AVER(SegBuffer(seg) == NULL);

  /* keep the destructions in step with AMSSegInit failure cases */
  amsDestroyTables(amsseg->allocTable, amsseg->nongreyTable,
                   amsseg->nonwhiteTable, arena, amsseg->grains);

  RingRemove(&amsseg->segRing);
  RingFinish(&amsseg->segRing);

  AVER(ams->size >= SegSize(seg));
  ams->size -= SegSize(seg);
  amsseg->sig = SigInvalid;

  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(AMSSegClass);
  super->finish(seg);
}


/* AMSSegMerge & AMSSegSplit -- AMSSeg split & merge methods
 *
 * .empty: segment merging and splitting is limited to simple cases
 * where the high segment is empty.
 * See design.mps.poolams.split-merge.constrain.
 *
 * .grain-align: segment merging and splitting is limited to cases
 * where the join is aligned with the grain alignment
 * See design.mps.poolams.split-merge.constrain.
 *
 * .alloc-early: Allocations are performed before calling the
 * next method to simplify the fail cases. See
 * design.mps.seg.split-merge.fail
 *
 * .table-names: The names of local variables holding the new
 * allocation and colour tables are chosen to have names which
 * are derivable from the field names for tables in AMSSegStruct.
 * (I.e. allocTable, nongreyTable, nonwhiteTable). This simplifies
 * processing of all such tables by a macro.
 */

static Res AMSSegMerge(Seg seg, Seg segHi,
                       Addr base, Addr mid, Addr limit,
                       Bool withReservoirPermit, va_list args)
{
  SegClass super;
  Count loGrains, hiGrains, allGrains;
  AMSSeg amsseg, amssegHi;
  Arena arena;
  AMS ams;
  BT allocTable, nongreyTable, nonwhiteTable;   /* .table-names */
  Res res;

  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  amsseg = Seg2AMSSeg(seg);
  amssegHi = Seg2AMSSeg(segHi);
  AVERT(AMSSeg, amsseg);
  AVERT(AMSSeg, amssegHi);
  /* other parameters are checked by next-method */
  arena = PoolArena(SegPool(seg));
  ams = Pool2AMS(SegPool(seg));

  loGrains = amsseg->grains;
  hiGrains = amssegHi->grains;
  allGrains = loGrains + hiGrains;

  /* checks for .grain-align */
  AVER(allGrains == AddrOffset(base, limit) >> ams->grainShift);
  /* checks for .empty */
  AVER(amssegHi->free == hiGrains);
  AVER(!amssegHi->marksChanged);

  /* .alloc-early  */
  res = amsCreateTables(&allocTable, &nongreyTable, &nonwhiteTable,
                        arena, allGrains);
  if (res != ResOK)
    goto failCreateTables;

  /* Merge the superclass fields via next-method call */
  super = SEG_SUPERCLASS(AMSSegClass);
  res = super->merge(seg, segHi, base, mid, limit,
                     withReservoirPermit, args);
  if (res != ResOK)
    goto failSuper;

  /* Update fields of seg. Finish segHi. */

#define MERGE_TABLES(table, setHighRangeFn) \
  /* Implementation depends on .table-names */ \
  BEGIN \
    BTCopyRange(amsseg->table, (table), 0, loGrains); \
    setHighRangeFn((table), loGrains, allGrains); \
    BTDestroy(amsseg->table, arena, loGrains); \
    BTDestroy(amssegHi->table, arena, hiGrains); \
    amsseg->table = (table); \
  END

  MERGE_TABLES(nonwhiteTable, BTSetRange);
  MERGE_TABLES(nongreyTable, BTSetRange);
  MERGE_TABLES(allocTable, BTResRange);

  amsseg->grains = allGrains;
  amsseg->free = amsseg->free + amssegHi->free;
  amsseg->newAlloc = amsseg->newAlloc + amssegHi->newAlloc;
  /* other fields in amsseg are unaffected */

  RingRemove(&amssegHi->segRing);
  RingFinish(&amssegHi->segRing);
  amssegHi->sig = SigInvalid;

  AVERT(AMSSeg, amsseg);
  return ResOK;

failSuper:
  amsDestroyTables(allocTable, nongreyTable, nonwhiteTable,
                   arena, allGrains);
failCreateTables:
  AVERT(AMSSeg, amsseg);
  AVERT(AMSSeg, amssegHi);
  return res;
}


static Res AMSSegSplit(Seg seg, Seg segHi,
                       Addr base, Addr mid, Addr limit,
                       Bool withReservoirPermit, va_list args)
{
  SegClass super;
  Count loGrains, hiGrains, allGrains;
  AMSSeg amsseg, amssegHi;
  Arena arena;
  AMS ams;
  BT allocTableLo, nongreyTableLo, nonwhiteTableLo; /* .table-names */
  BT allocTableHi, nongreyTableHi, nonwhiteTableHi; /* .table-names */
  Res res;

  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  amsseg = Seg2AMSSeg(seg);
  amssegHi = Seg2AMSSeg(segHi);
  AVERT(AMSSeg, amsseg);
  /* other parameters are checked by next-method */
  arena = PoolArena(SegPool(seg));
  ams = Pool2AMS(SegPool(seg));

  loGrains = AMSGrains(ams, AddrOffset(base, mid));
  hiGrains = AMSGrains(ams, AddrOffset(mid, limit));
  allGrains = loGrains + hiGrains;

  /* checks for .grain-align */
  AVER(allGrains == amsseg->grains);
  /* checks for .empty */
  AVER(amsseg->free >= hiGrains);
  if (amsseg->allocTableInUse) {
    AVER(BTIsResRange(amsseg->allocTable, loGrains, allGrains));
  } else {
    AVER(amsseg->firstFree <= loGrains);
  }

  /* .alloc-early */
  res = amsCreateTables(&allocTableLo, &nongreyTableLo, &nonwhiteTableLo,
                        arena, loGrains);
  if (res != ResOK)
    goto failCreateTablesLo;
  res = amsCreateTables(&allocTableHi, &nongreyTableHi, &nonwhiteTableHi,
                        arena, hiGrains);
  if (res != ResOK)
    goto failCreateTablesHi;


  /* Split the superclass fields via next-method call */
  super = SEG_SUPERCLASS(AMSSegClass);
  res = super->split(seg, segHi, base, mid, limit, withReservoirPermit, args);
  if (res != ResOK)
    goto failSuper;

  /* Update seg. Full initialization for segHi. */

#define SPLIT_TABLES(table, setHighRangeFn) \
  /* Implementation depends on .table-names */ \
  BEGIN \
    BTCopyRange(amsseg->table, table ## Lo, 0, loGrains); \
    setHighRangeFn(table ## Hi, 0, hiGrains); \
    BTDestroy(amsseg->table, arena, allGrains); \
    amsseg->table = table ## Lo; \
    amssegHi->table = table ## Hi; \
  END

  SPLIT_TABLES(nonwhiteTable, BTSetRange);
  SPLIT_TABLES(nongreyTable, BTSetRange);
  SPLIT_TABLES(allocTable, BTResRange);

  amsseg->grains = loGrains;
  amssegHi->grains = hiGrains;
  amsseg->free -= hiGrains;
  amssegHi->free = hiGrains;
  amssegHi->newAlloc = (Count)0;
  amssegHi->marksChanged = FALSE; /* design.mps.poolams.marked.unused */
  amssegHi->ambiguousFixes = FALSE;

  /* start off using firstFree, see design.mps.poolams.no-bit */
  amssegHi->allocTableInUse = FALSE;
  amssegHi->firstFree = 0;
  /* use colour tables if the segment is white */
  amssegHi->colourTablesInUse = (SegWhite(segHi) != TraceSetEMPTY);

  amssegHi->ams = ams;
  RingInit(&amssegHi->segRing);
  RingAppend((ams->allocRing)(ams, SegRankSet(segHi), SegSize(segHi)),
             &amssegHi->segRing);

  amssegHi->sig = AMSSegSig;
  AVERT(AMSSeg, amsseg);
  AVERT(AMSSeg, amssegHi);
  return ResOK;

failSuper:
  amsDestroyTables(allocTableHi, nongreyTableHi, nonwhiteTableHi,
                   arena, hiGrains);
failCreateTablesHi:
  amsDestroyTables(allocTableLo, nongreyTableLo, nonwhiteTableLo,
                   arena, loGrains);
failCreateTablesLo:
  AVERT(AMSSeg, amsseg);
  return res;
}


/* AMSSegDescribe -- describe an AMS segment */

#define WRITE_BUFFER_LIMIT(stream, seg, i, buffer, accessor, char) \
  BEGIN \
    if ((buffer) != NULL \
       && (i) == AMS_ADDR_INDEX(seg, accessor(buffer))) { \
      Res _res = WriteF(stream, char, NULL); \
      if (_res != ResOK) return _res; \
    } \
  END

static Res AMSSegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  AMSSeg amsseg;
  SegClass super;
  Buffer buffer;               /* the segment's buffer, if it has one */
  Index i;

  if (!CHECKT(Seg, seg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  amsseg = Seg2AMSSeg(seg);
  if (!CHECKT(AMSSeg, amsseg)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(AMSSegClass);
  res = super->describe(seg, stream);
  if (res != ResOK) return res;

  buffer = SegBuffer(seg);

  res = WriteF(stream,
               "  AMS $P\n", (WriteFP)amsseg->ams,
               "  grains $W\n", (WriteFW)amsseg->grains,
               NULL);
  if (res != ResOK) return res;
  if (amsseg->allocTableInUse)
    res = WriteF(stream,
                 "  alloctable $P\n", (WriteFP)amsseg->allocTable,
                 NULL);
  else
    res = WriteF(stream,
                 "  firstFree $W\n", (WriteFW)amsseg->firstFree,
                 NULL);
  if (res != ResOK) return res;
  res = WriteF(stream,
               "  tables: nongrey $P, nonwhite $P\n",
               (WriteFP)amsseg->nongreyTable,
               (WriteFP)amsseg->nonwhiteTable,
               "  map: \n",
               NULL);
  if (res != ResOK) return res;

  for (i=0; i < amsseg->grains; ++i) {
    char c = 0;

    if (i % 64 == 0) {
      res = WriteF(stream, "\n  ", NULL);
      if (res != ResOK) return res;
    }

    WRITE_BUFFER_LIMIT(stream, seg, i, buffer, BufferBase, "[");
    WRITE_BUFFER_LIMIT(stream, seg, i, buffer, BufferGetInit, "|");
    WRITE_BUFFER_LIMIT(stream, seg, i, buffer, BufferAlloc, ">");

    if (AMS_ALLOCED(seg, i)) {
      if (amsseg->colourTablesInUse) {
        if (AMSIsInvalidColor(seg, i))
          c = '!';
        else if (AMSIsWhite(seg, i))
          c = '-';
        else if (AMSIsGrey(seg, i))
          c = '+';
        else /* must be black */
          c = '*';
      } else
        c = '.';
    } else
      c = ' ';
    res = WriteF(stream, "$C", c, NULL);
    if (res != ResOK)
      return res;

    WRITE_BUFFER_LIMIT(stream, seg, i+1, buffer, BufferScanLimit, "<");
    WRITE_BUFFER_LIMIT(stream, seg, i+1, buffer, BufferLimit, "]");
  }

  res = WriteF(stream, "\n", NULL);
  return res;
}


/* AMSSegClass -- Class definition for AMS segments */

DEFINE_CLASS(AMSSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  class->name = "AMSSEG";
  class->size = sizeof(AMSSegStruct);
  class->init = AMSSegInit;
  class->finish = AMSSegFinish;
  class->merge = AMSSegMerge;
  class->split = AMSSegSplit;
  class->describe = AMSSegDescribe;
}




/* AMSPoolRing -- the ring of segments in the pool */

static Ring AMSPoolRing(AMS ams, RankSet rankSet, Size size)
{
  /* arguments checked in the caller */
  UNUSED(rankSet); UNUSED(size);
  return &ams->segRing;
}


/* AMSSegSizePolicy
 *
 * Picks a segment size.  This policy simply rounds the size
 * up to the arena alignment.
 */
static Res AMSSegSizePolicy(Size *sizeReturn,
                            Pool pool, Size size, RankSet rankSet)
{
  Arena arena;

  AVER(sizeReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(RankSetCheck(rankSet));

  arena = PoolArena(pool);

  size = SizeAlignUp(size, ArenaAlign(arena));
  if (size == 0) {
    /* overflow */
    return ResMEMORY;
  }
  *sizeReturn = size;
  return ResOK;
}


/* AMSSegCreate -- create a single AMSSeg */

static Res AMSSegCreate(Seg *segReturn, Pool pool, Size size,
                        SegPref segPref, RankSet rankSet,
                        Bool withReservoirPermit)
{
  Seg seg;
  AMS ams;
  Res res;
  Arena arena;
  Size prefSize;

  AVER(segReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(RankSet, rankSet);
  AVERT(SegPref, segPref);
  AVER(BoolCheck(withReservoirPermit));

  ams = Pool2AMS(pool);
  AVERT(AMS,ams);
  arena = PoolArena(pool);

  res = ams->segSize(&prefSize, pool, size, rankSet);
  if (res != ResOK)
    goto failSize;

  res = SegAlloc(&seg, (*ams->segClass)(), segPref, prefSize,
                 pool, withReservoirPermit);
  if (res != ResOK) { /* try to allocate one that's just large enough */
    Size minSize = SizeAlignUp(size, ArenaAlign(arena));

    if (minSize == prefSize)
      goto failSeg;
    res = SegAlloc(&seg, (*ams->segClass)(), segPref, minSize,
                   pool, withReservoirPermit);
    if (res != ResOK)
      goto failSeg;
  }
  PoolGenUpdateZones(&ams->pgen, seg);

  /* see design.mps.seg.field.rankset */
  if (rankSet != RankSetEMPTY) {
    SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  } else {
    SegSetRankAndSummary(seg, rankSet, RefSetEMPTY);
  }

  AVERT(AMSSeg, Seg2AMSSeg(seg));

  *segReturn = seg;
  return ResOK;

failSeg:
failSize:
  return res;
}


/* AMSSegsDestroy -- destroy all the segments */

static void AMSSegsDestroy(AMS ams)
{
  Ring ring, node, next;     /* for iterating over the segments */

  ring = PoolSegRing(AMS2Pool(ams));
  RING_FOR(node, ring, next) {
    Seg seg = SegOfPoolRing(node);
    AVER(Seg2AMSSeg(seg)->ams == ams);
    SegFree(seg);
  }
}


static Res AMSIterate(Seg seg, AMSObjectFunction f, void *closure);


/* AMSInit -- the pool class initialization method
 *
 *  Takes one additional argument: the format of the objects
 *  allocated in the pool.  See design.mps.poolams.init.
 */
static Res AMSInit(Pool pool, va_list args)
{
  Res res;
  Format format;
  Chain chain;

  AVERT(Pool, pool);

  format = va_arg(args, Format);
  chain = va_arg(args, Chain);
  res = AMSInitInternal(Pool2AMS(pool), format, chain);
  if (res == ResOK) {
    EVENT_PPP(PoolInitAMS, pool, PoolArena(pool), format);
  }
  return res;
}


/* AMSInitInternal -- initialize an AMS pool, given the format and the chain */

Res AMSInitInternal(AMS ams, Format format, Chain chain)
{
  Pool pool;
  Res res;

  /* Can't check ams, it's not initialized. */
  AVERT(Format, format);
  AVERT(Chain, chain);

  pool = AMS2Pool(ams);
  AVERT(Pool, pool);
  pool->format = format;
  pool->alignment = pool->format->alignment;
  ams->grainShift = SizeLog2(PoolAlignment(pool));

  if (ChainGens(chain) != 1)
    return ResPARAM;
  ams->chain = chain;
  res = PoolGenInit(&ams->pgen, ams->chain, 0, pool);
  if (res != ResOK)
    return res;

  RingInit(&ams->segRing);

  /* The next five might be overridden by a subclass. */
  ams->iterate = AMSIterate; /* should be done using a format variant */
  ams->segSize = AMSSegSizePolicy;
  ams->allocRing = AMSPoolRing;
  ams->segsDestroy = AMSSegsDestroy;
  ams->segClass = AMSSegClassGet;

  ams->size = 0;

  ams->sig = AMSSig;
  AVERT(AMS, ams);
  return ResOK;
}


/* AMSFinish -- the pool class finishing method
 *
 * Destroys all the segs in the pool.  Can't invalidate the AMS until
 * we've destroyed all the segments, as it may be checked.
 */
void AMSFinish(Pool pool)
{
  AMS ams;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);

  (ams->segsDestroy)(ams);
  /* can't invalidate the AMS until we've destroyed all the segs */
  ams->sig = SigInvalid;
  RingFinish(&ams->segRing);
  PoolGenFinish(&ams->pgen);
}


/* amsSegAlloc -- try to allocate an area in the given segment
 *
 * Tries to find an area of at least the given size.  If successful,
 * makes that area black, if necessary, and returns its base and limit
 * grain indices.
 */
static Bool amsSegAlloc(Index *baseReturn, Index *limitReturn,
                        Seg seg, Size size)
{
  AMS ams;
  AMSSeg amsseg;
  Size grains;
  Bool canAlloc;      /* can we allocate in this segment? */
  Index base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  /* seg has already been checked, in AMSBufferFill. */
  amsseg = Seg2AMSSeg(seg);

  ams = amsseg->ams;
  AVERT(AMS, ams);

  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(AMS2Pool(ams))));

  grains = AMSGrains(ams, size);
  AVER(grains > 0);
  if (grains > amsseg->grains)
    return FALSE;

  if (amsseg->allocTableInUse) {
    canAlloc = BTFindLongResRange(&base, &limit, amsseg->allocTable,
                                  0, amsseg->grains, grains);
    if (!canAlloc)
      return FALSE;
    BTSetRange(amsseg->allocTable, base, limit);
  } else {
    if (amsseg->firstFree > amsseg->grains - grains)
      return FALSE;
    base = amsseg->firstFree; limit = amsseg->grains;
    amsseg->firstFree = limit;
  }

  amsseg->free -= limit - base;
  amsseg->newAlloc += limit - base;
  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


/* AMSBufferFill -- the pool class buffer fill method
 *
 * Iterates over the segments looking for space.  See
 * design.mps.poolams.fill.
 */
Res AMSBufferFill(Addr *baseReturn, Addr *limitReturn,
                  Pool pool, Buffer buffer, Size size,
                  Bool withReservoirPermit)
{
  Res res;
  AMS ams;
  Seg seg;
  Ring node, ring, nextNode;    /* for iterating over the segments */
  Index base, limit;
  RankSet rankSet;
  Bool b;                       /* the return value of amsSegAlloc */
  SegPrefStruct segPrefStruct;
  Size allocatedSize;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVER(BoolCheck(withReservoirPermit));

  /* Check that we're not in the grey mutator phase (see */
  /* design.mps.poolams.fill.colour). */
  AVER(PoolArena(pool)->busyTraces == PoolArena(pool)->flippedTraces);

  rankSet = BufferRankSet(buffer);
  ring = (ams->allocRing)(ams, rankSet, size);
  /* design.mps.poolams.fill.slow */
  RING_FOR(node, ring, nextNode) {
    AMSSeg amsseg = RING_ELT(AMSSeg, segRing, node);
    AVERT_CRITICAL(AMSSeg, amsseg);
    if (amsseg->free >= AMSGrains(ams, size)) {
      seg = AMSSeg2Seg(amsseg);

      if (SegRankSet(seg) == rankSet && SegBuffer(seg) == NULL) {
        b = amsSegAlloc(&base, &limit, seg, size);
        if (b)
          goto found;
      }
    }
  }

  /* no segment has enough room; make a new segment */
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  res = AMSSegCreate(&seg, pool, size, &segPrefStruct, rankSet,
                     withReservoirPermit);
  if (res != ResOK)
    return res;
  b = amsSegAlloc(&base, &limit, seg, size);

found:
  AVER(b);
  allocatedSize = AddrOffset(AMS_INDEX_ADDR(seg, base),
                             AMS_INDEX_ADDR(seg, limit));
  ams->pgen.totalSize += allocatedSize;
  ams->pgen.newSize += allocatedSize;

  *baseReturn = AMS_INDEX_ADDR(seg, base);
  *limitReturn = AMS_INDEX_ADDR(seg, limit);
  return ResOK;
}


/* AMSBufferEmpty -- the pool class buffer empty method
 *
 * Frees the unused part of the buffer.  The colour of the area doesn't
 * need to be changed.  See design.mps.poolams.empty.
 */
void AMSBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  AMS ams;
  Index initIndex, limitIndex;
  Seg seg;
  AMSSeg amsseg;
  Size size;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer,buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVER(SegCheck(seg));
  AVER(init <= limit);
  AVER(AddrIsAligned(init, PoolAlignment(pool)));
  AVER(AddrIsAligned(limit, PoolAlignment(pool)));

  amsseg = Seg2AMSSeg(seg);
  AVERT(AMSSeg, amsseg);

  if (init == limit)
    return;

  initIndex = AMS_ADDR_INDEX(seg, init);
  limitIndex = AMS_ADDR_INDEX(seg, limit);

  if (amsseg->allocTableInUse) {
    /* check that it's allocated */
    AVER(BTIsSetRange(amsseg->allocTable, initIndex, limitIndex));
    BTResRange(amsseg->allocTable, initIndex, limitIndex);
  } else {
    /* check that it's allocated */
    AVER(limitIndex <= amsseg->firstFree);
    if (limitIndex == amsseg->firstFree) /* is it at the end? */ {
      amsseg->firstFree = initIndex;
    } else { /* start using allocTable */
      amsseg->allocTableInUse = TRUE;
      BTSetRange(amsseg->allocTable, 0, amsseg->firstFree);
      if (amsseg->firstFree < amsseg->grains)
        BTResRange(amsseg->allocTable, amsseg->firstFree, amsseg->grains);
      BTResRange(amsseg->allocTable, initIndex, limitIndex);
    }
  }
  amsseg->free += limitIndex - initIndex;
  /* The unused portion of the buffer must be new, since it's not condemned. */
  AVER(amsseg->newAlloc >= limitIndex - initIndex);
  amsseg->newAlloc -= limitIndex - initIndex;
  size = AddrOffset(init, limit);
  ams->pgen.totalSize -= size;
  ams->pgen.newSize -= size;
}


/* amsRangeCondemn -- Condemn a part of an AMS segment
 *
 * I.e., alloc -> white, free -> black.
 * Allow calling it with base = limit, to simplify the callers.
 */
static void amsRangeCondemn(Seg seg, Index base, Index limit)
{
  if (base != limit) {
    AMSSeg amsseg = Seg2AMSSeg(seg);

    AVER(base < limit);
    AVER(limit <= amsseg->grains);

    if (amsseg->allocTableInUse) {
      BTSetRange(amsseg->nongreyTable, base, limit);
      BTCopyInvertRange(amsseg->allocTable, amsseg->nonwhiteTable,
                        base, limit);
    } else {
      if (base < amsseg->firstFree) {
        AMSRangeWhiten(seg, base, amsseg->firstFree);
      }
      if (amsseg->firstFree < limit) {
        AMSRangeBlacken(seg, amsseg->firstFree, limit);
      }
    }
  }
}


/* AMSWhiten -- the pool class segment condemning method */

Res AMSWhiten(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSSeg amsseg;
  Buffer buffer;                /* the seg's buffer, if it has one */
  Count uncondemned;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);

  AVERT(Trace, trace);
  AVER(SegCheck(seg));

  amsseg = Seg2AMSSeg(seg);
  AVERT(AMSSeg, amsseg);

  /* design.mps.poolams.colour.single */
  AVER(SegWhite(seg) == TraceSetEMPTY);
  AVER(!amsseg->colourTablesInUse);

  amsseg->colourTablesInUse = TRUE;
  buffer = SegBuffer(seg);
  if (buffer != NULL) { /* design.mps.poolams.condemn.buffer */
    Index scanLimitIndex, limitIndex;
    scanLimitIndex = AMS_ADDR_INDEX(seg, BufferScanLimit(buffer));
    limitIndex = AMS_ADDR_INDEX(seg, BufferLimit(buffer));

    amsRangeCondemn(seg, 0, scanLimitIndex);
    if (scanLimitIndex < limitIndex)
      AMSRangeBlacken(seg, scanLimitIndex, limitIndex);
    amsRangeCondemn(seg, limitIndex, amsseg->grains);
    /* We didn't condemn the buffer, subtract it from the count. */
    uncondemned = limitIndex - scanLimitIndex;
  } else { /* condemn whole seg */
    amsRangeCondemn(seg, 0, amsseg->grains);
    uncondemned = (Count)0;
  }

  trace->condemned += SegSize(seg) - AMSGrainsSize(ams, uncondemned);
  /* The unused part of the buffer is new allocation by definition. */
  ams->pgen.newSize -= AMSGrainsSize(ams, amsseg->newAlloc - uncondemned);
  amsseg->newAlloc = uncondemned;
  amsseg->marksChanged = FALSE; /* design.mps.poolams.marked.condemn */
  amsseg->ambiguousFixes = FALSE;

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));

  return ResOK;
}



/* AMSIterate -- applies a function to each object in a segment
 *
 * AMSIterate(seg, f, closure) applies f to all the
 * objects in the segment.  It skips the buffer, if any (from
 * BufferScanLimit to BufferLimit).
 */
static Res AMSIterate(Seg seg, AMSObjectFunction f, void *closure)
{
  Res res;
  AMS ams;
  AMSSeg amsseg;
  Format format;
  Align alignment;
  Index i;
  Addr p, next, limit;
  Buffer buffer;

  AVERT(Seg, seg);
  AVERT(AMSObjectFunction, f);
  /* Can't check closure */

  amsseg = Seg2AMSSeg(seg);
  AVERT(AMSSeg, amsseg);
  ams = amsseg->ams;
  AVERT(AMS, ams);
  format = AMS2Pool(ams)->format;
  AVERT(Format, format);
  alignment = PoolAlignment(AMS2Pool(ams));

  p = SegBase(seg);
  limit = SegLimit(seg);
  buffer = SegBuffer(seg);

  while (p < limit) { /* loop over the objects in the segment */
    if (buffer != NULL
       && p == BufferScanLimit(buffer) && p != BufferLimit(buffer)) {
      /* skip buffer */
      next = BufferLimit(buffer);
      AVER(AddrIsAligned(next, alignment));
    } else {
      AVER((buffer == NULL)
	   || (p < BufferScanLimit(buffer))
	   || (p >= BufferLimit(buffer)));  /* not in the buffer */

      i = AMS_ADDR_INDEX(seg, p);
      if (!AMS_ALLOCED(seg, i)) { /* no object here */
        next = AddrAdd(p, alignment); /* @@@@ this could be improved */
      } else { /* there is an object here */
        next = (*format->skip)(AddrAdd(p, format->headerSize));
        next = AddrSub(next, format->headerSize);
        AVER(AddrIsAligned(next, alignment));
	res = (*f)(seg, i, p, next, closure);
	if (res != ResOK)
	  return res;
      }
    }
    AVER(next > p); /* make sure we make progress */
    p = next;
  }
  AVER(p == limit);
  return ResOK;
}


/* amsScanObject -- scan a single object
 *
 * This is the object function passed to AMSIterate by AMSScan.
 */

struct amsScanClosureStruct {
  ScanState ss;
  Bool scanAllObjects;
};

typedef struct amsScanClosureStruct *amsScanClosure;

static Res amsScanObject(Seg seg, Index i, Addr p, Addr next, void *clos)
{
  amsScanClosure closure;
  AMSSeg amsseg;
  Format format;
  Res res;

  amsseg = Seg2AMSSeg(seg);
  /* seg & amsseg have already been checked, in AMSIterate. */
  AVER(i < amsseg->grains);
  AVER(p != 0);
  AVER(p < next);
  AVER(clos != NULL);
  closure = clos;
  AVERT(ScanState, closure->ss);
  AVER(BoolCheck(closure->scanAllObjects));

  format = AMS2Pool(amsseg->ams)->format;
  AVERT(Format, format);

  /* @@@@ This isn't quite right for multiple traces. */
  if (closure->scanAllObjects || AMSIsGrey(seg, i)) {
    res = (*format->scan)(closure->ss,
                          AddrAdd(p, format->headerSize),
                          AddrAdd(next, format->headerSize));
    if (res != ResOK)
      return res;
    closure->ss->scannedSize += AddrOffset(p, next);
    if (!closure->scanAllObjects) {
      Index j = AMS_ADDR_INDEX(seg, next);
      AVER(!AMSIsInvalidColor(seg, i));
      AMSGreyBlacken(seg, i);
      if (i+1 < j)
        AMSRangeWhiteBlacken(seg, i+1, j);
    }
  }

  return ResOK;
}


/* AMSScan -- the pool class segment scanning method
 *
 * See design.mps.poolams.scan
 */
Res AMSScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Res res;
  AMS ams;
  Arena arena;
  AMSSeg amsseg;
  struct amsScanClosureStruct closureStruct;
  Format format;
  Align alignment;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  arena = PoolArena(pool);
  AVER(SegCheck(seg));
  amsseg = Seg2AMSSeg(seg);
  AVERT(AMSSeg, amsseg);

  /* Check that we're not in the grey mutator phase (see */
  /* design.mps.poolams.not-req.grey). */
  AVER(TraceSetSub(ss->traces, arena->flippedTraces));

  closureStruct.scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);
  closureStruct.ss = ss;
  /* @@@@ This isn't quite right for multiple traces. */
  if (closureStruct.scanAllObjects) {
    /* The whole seg (except the buffer) is grey for some trace. */
    res = (ams->iterate)(seg, amsScanObject, &closureStruct);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    *totalReturn = TRUE;
  } else {
    AVER(amsseg->marksChanged); /* something must have changed */
    AVER(amsseg->colourTablesInUse);
    format = pool->format;
    AVERT(Format, format);
    alignment = PoolAlignment(AMS2Pool(ams));
    do { /* design.mps.poolams.scan.iter */
      amsseg->marksChanged = FALSE; /* design.mps.poolams.marked.scan */
      /* design.mps.poolams.ambiguous.middle */
      if (amsseg->ambiguousFixes) {
        res = (ams->iterate)(seg, amsScanObject, &closureStruct);
        if (res != ResOK) {
          /* design.mps.poolams.marked.scan.fail */
          amsseg->marksChanged = TRUE;
          *totalReturn = FALSE;
          return res;
        }
      } else {
        Index i, j = 0;
        Addr p, next;

        while(j < amsseg->grains
              && AMSFindGrey(&i, &j, seg, j, amsseg->grains)) {
            Addr clientP, clientNext;
          AVER(!AMSIsInvalidColor(seg, i));
          p = AMS_INDEX_ADDR(seg, i);
          clientP = AddrAdd(p, format->headerSize);
          clientNext = (*format->skip)(clientP);
          next = AddrSub(clientNext, format->headerSize);
          AVER(AddrIsAligned(next, alignment));
          j = AMS_ADDR_INDEX(seg, next);
          res = (*format->scan)(ss, clientP, clientNext);
          if (res != ResOK) {
            /* design.mps.poolams.marked.scan.fail */
            amsseg->marksChanged = TRUE;
            *totalReturn = FALSE;
            return res;
          }
          ss->scannedSize += AddrOffset(p, next);
          AMSGreyBlacken(seg, i);
          if (i+1 < j)
            AMSRangeWhiteBlacken(seg, i+1, j);
        }
      }
    } while(amsseg->marksChanged);
    *totalReturn = FALSE;
  }

  return ResOK;
}


/* AMSFix -- the pool class fixing method */

Res AMSFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AMSSeg amsseg;
  Index i;                      /* the index of the fixed grain */
  Addr base;
  Ref clientRef;
  Format format;

  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(CHECKT(AMS, Pool2AMS(pool)));
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);

  format = pool->format;
  AVERT(Format, format);

  amsseg = Seg2AMSSeg(seg);
  AVERT_CRITICAL(AMSSeg, amsseg);
  /* It's a white seg, so it must have colour tables. */
  AVER_CRITICAL(amsseg->colourTablesInUse);

  /* @@@@ We should check that we're not in the grey mutator phase */
  /* (see design.mps.poolams.not-req.grey), but there's no way of */
  /* doing that here (this can be called from RootScan, during flip). */

  clientRef = *refIO;
  base = AddrSub((Addr)clientRef, format->headerSize);
  /* can get an ambiguous reference too close to the base of the
   * segment, so when we subtract the header we are not in the
   * segment any longer.  This isn't a real reference,
   * so we can just skip it.  */
  if (base < SegBase(seg)) {
      return ResOK;
  }

  i = AMS_ADDR_INDEX(seg, base);
  AVER_CRITICAL(!AMSIsInvalidColor(seg, i));

  ss->wasMarked = TRUE;

  switch (ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if (!AddrIsAligned(base, PoolAlignment(pool))
       || !AMS_ALLOCED(seg, i)) {
      break;
    }
    amsseg->ambiguousFixes = TRUE;
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    AVER_CRITICAL(AddrIsAligned(base, PoolAlignment(pool)));
    AVER_CRITICAL(AMS_ALLOCED(seg, i));
    if (AMSIsWhite(seg, i)) {
      ss->wasMarked = FALSE;
      if (ss->rank == RankWEAK) { /* then splat the reference */
        *refIO = (Ref)0;
      } else {
        ++ss->preservedInPlaceCount; /* Size updated on reclaim */
        if (SegRankSet(seg) == RankSetEMPTY && ss->rank != RankAMBIG) {
          /* design.mps.poolams.fix.to-black */
          Addr clientNext, next;

          ShieldExpose(PoolArena(pool), seg);
          clientNext = (*pool->format->skip)(clientRef);
          ShieldCover(PoolArena(pool), seg);
          next = AddrSub(clientNext, format->headerSize);
          /* Part of the object might be grey, because of ambiguous */
          /* fixes, but that's OK, because scan will ignore that. */
          AMSRangeWhiteBlacken(seg, i, AMS_ADDR_INDEX(seg, next));
        } else { /* turn it grey */
          AMSWhiteGreyen(seg, i);
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
          /* mark it for scanning - design.mps.poolams.marked.fix */
          amsseg->marksChanged = TRUE;
        }
      }
    }
    break;
  default:
    NOTREACHED;
  }

  return ResOK;
}


/* AMSBlacken -- the pool class blackening method
 *
 * Turn all grey objects black.
 */
void AMSBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AMS ams;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);

  /* If it's white for any trace, remove the greyness from tables. */
  if (TraceSetInter(traceSet, SegWhite(seg)) != TraceSetEMPTY) {
    AMSSeg amsseg = Seg2AMSSeg(seg);
    AVERT(AMSSeg, amsseg);
    AVER(amsseg->marksChanged); /* there must be something grey */
    amsseg->marksChanged = FALSE;
    /* This will turn grey->black, and not affect black or white. */
    BTSetRange(amsseg->nongreyTable, 0, amsseg->grains);
  }
}


/* AMSReclaim -- the pool class reclamation method */

void AMSReclaim(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSSeg amsseg;
  Format format;
  Align alignment;
  Count reclaimed = 0;
  Index i, j = 0;
  Addr p, next;

  AVERT(Pool, pool);
  ams = Pool2AMS(pool);
  AVERT(AMS, ams);
  AVERT(Seg, seg);

  amsseg = Seg2AMSSeg(seg);
  /* It's a white seg, so it must have colour tables. */
  AVER(amsseg->colourTablesInUse);
  AVER(!amsseg->marksChanged); /* there must be nothing grey */
  format = pool->format;
  AVERT(Format, format);
  alignment = PoolAlignment(AMS2Pool(ams));

  /* Start using allocTable */
  if (!amsseg->allocTableInUse) {
    amsseg->allocTableInUse = TRUE;
    if (0 < amsseg->firstFree)
      BTSetRange(amsseg->allocTable, 0, amsseg->firstFree);
    if (amsseg->firstFree < amsseg->grains)
      BTResRange(amsseg->allocTable, amsseg->firstFree, amsseg->grains);
  }

  /* Loop over all white objects and free them */
  while(j < amsseg->grains
        && AMSFindWhite(&i, &j, seg, j, amsseg->grains)) {
    Addr clientP, clientNext;
    AVER(!AMSIsInvalidColor(seg, i));
    p = AMS_INDEX_ADDR(seg, i);
    clientP = AddrAdd(p, format->headerSize);
    clientNext = (*format->skip)(clientP);
    next = AddrSub(clientNext, format->headerSize);
    AVER(AddrIsAligned(next, alignment));
    j = AMS_ADDR_INDEX(seg, next);
    BTResRange(amsseg->allocTable, i, j);
    reclaimed += j - i;
  }

  amsseg->free += reclaimed;
  trace->reclaimSize += reclaimed << ams->grainShift;
  ams->pgen.totalSize -= reclaimed << ams->grainShift;
  /* preservedInPlaceCount is updated on fix */
  trace->preservedInPlaceSize +=
    (amsseg->grains - amsseg->free) << ams->grainShift;

  if (amsseg->free == amsseg->grains && SegBuffer(seg) == NULL) {
    /* No survivors */
    SegFree(seg);
  } else {
    amsseg->colourTablesInUse = FALSE;
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  }
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

  if (!CHECKT(Pool, pool)) return ResFAIL;
  ams = Pool2AMS(pool);
  if (!CHECKT(AMS, ams)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               (WriteFP)pool, (WriteFU)pool->serial,
               "  size $W\n",
               (WriteFW)ams->size,
               "  grain shift $U\n", (WriteFU)ams->grainShift,
               "  chain $P\n",
               (WriteFP)ams->chain,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  segments\n"
               "    * = black, + = grey, - = white, . = alloc, ! = bad\n"
               "    buffers: [ = base, < = scan limit, | = init,\n"
               "             > = alloc, ] = limit\n",
               NULL);
  if (res != ResOK) return res;

  RING_FOR(node, &ams->segRing, nextNode) {
    AMSSeg amsseg = RING_ELT(AMSSeg, segRing, node);
    res = SegDescribe(AMSSeg2Seg(amsseg), stream);
    if (res != ResOK) return res;
  }
  return ResOK;
}


/* AMSPoolClass -- the class definition */

/* impl.h.poolams contains the type definition.  Hence the use */
/* of DEFINE_CLASS rather than DEFINE_POOL_CLASS */

DEFINE_CLASS(AMSPoolClass, this)
{
  INHERIT_CLASS(this, AbstractCollectPoolClass);
  PoolClassMixInFormat(this);
  this->name = "AMS";
  this->size = sizeof(AMSStruct);
  this->offset = offsetof(AMSStruct, poolStruct);
  this->init = AMSInit;
  this->finish = AMSFinish;
  this->bufferClass = RankBufClassGet;
  this->bufferFill = AMSBufferFill;
  this->bufferEmpty = AMSBufferEmpty;
  this->whiten = AMSWhiten;
  this->blacken = AMSBlacken;
  this->scan = AMSScan;
  this->fix = AMSFix;
  this->fixEmergency = AMSFix;
  this->reclaim = AMSReclaim;
  this->describe = AMSDescribe;
}


/* AMSCheck -- the check method for an AMS */

Bool AMSCheck(AMS ams)
{
  CHECKS(AMS, ams);
  CHECKD(Pool, AMS2Pool(ams));
  CHECKL(IsSubclassPoly(AMS2Pool(ams)->class, AMSPoolClassGet()));
  CHECKL(PoolAlignment(AMS2Pool(ams)) == ((Size)1 << ams->grainShift));
  CHECKL(PoolAlignment(AMS2Pool(ams)) == AMS2Pool(ams)->format->alignment);
  CHECKD(Chain, ams->chain);
  CHECKD(PoolGen, &ams->pgen);
  CHECKL(SizeIsAligned(ams->size, ArenaAlign(PoolArena(AMS2Pool(ams)))));
  CHECKL(ams->iterate != NULL);
  CHECKL(RingCheck(&ams->segRing));
  CHECKL(ams->allocRing != NULL);
  CHECKL(ams->segsDestroy != NULL);
  CHECKL(ams->segClass != NULL);

  return TRUE;
}


/* mps_class_ams -- return the pool class descriptor to the client */

mps_class_t mps_class_ams(void)
{
  return (mps_class_t)AMSPoolClassGet();
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

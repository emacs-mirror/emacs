/* impl.c.poolamc: AUTOMATIC MOSTLY-COPYING MEMORY POOL CLASS
 *
 * $HopeName: MMsrc!poolamc.c(trunk.53) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 *
 * .sources: design.mps.poolamc.
 */

#include "mpscamc.h"
#include "chain.h"
#include "mpm.h"

SRCID(poolamc, "$HopeName: MMsrc!poolamc.c(trunk.53) $");


/* PType enumeration -- distinguishes AMCGen and AMCNailBoard */
enum {AMCPTypeGen = 1, AMCPTypeNailBoard};

/* AMC typedef */
typedef struct AMCStruct *AMC;

/* AMCGen typedef */
typedef struct AMCGenStruct *AMCGen;

/* forward declarations */

static Bool AMCCheck(AMC amc);
static Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
static Res AMCHeaderFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
extern PoolClass EnsureAMCPoolClass(void);
extern BufferClass EnsureAMCBufClass(void);
extern SegClass EnsureAMCSegClass(void);


/* AMCGenStruct -- pool AMC generation descriptor */

#define AMCGenSig       ((Sig)0x519A3C9E)  /* SIGnature AMC GEn */

typedef struct AMCGenStruct {
  PoolGenStruct pgen;
  int type;                     /* AMCPTypeGen for a gen */
  RingStruct amcRing;           /* link in list of gens in pool */
  Buffer forward;               /* forwarding buffer */
  Count segs;                   /* number of segs in gen */
  Sig sig;                      /* impl.h.misc.sig */
} AMCGenStruct;

#define AMCGenAMC(amcgen) PoolPoolAMC((amcgen)->pgen.pool)
#define AMCGenPool(amcgen) ((amcgen)->pgen.pool)

#define AMCGenNr(amcgen) ((amcgen)->pgen.nr)


enum {outsideRamp = 1, beginRamp, ramping, finishRamp, collectingRamp};


/* AMCNailBoard -- the nail board */

typedef struct AMCNailBoardStruct *AMCNailBoard;
typedef struct AMCNailBoardStruct {
  Sig sig;
  int type;         /* AMCPTypeNailBoard for a nail board */
  AMCGen gen;       /* generation of this segment */
  Count nails;      /* number of ambigFixes, not necessarily distinct */
  Count distinctNails; /* number of distinct ambigFixes */
  Bool newMarks;    /* set to TRUE if a new mark bit is added */
  Shift markShift;  /* shift to convert offset into bit index for mark */
  BT mark;          /* mark table used to record ambiguous fixes */
} AMCNailBoardStruct;

#define AMCNailBoardSig ((Sig)0x519A3C4B) /* SIGnature AMC NailBoard */


/* AMCGSegStruct -- AMC segment structure 
 *
 * .segtype: AMC segs have a pointer to the type field of either
 * a nailboard or a generation. This initial value is passed 
 * as an additional parameter when the segment is allocated.
 * See design.mps.poolamc.fix.nail.distinguish.
 */

typedef struct AMCSegStruct *AMCSeg;

#define AMCSegSig      ((Sig)0x519A3C59) /* SIGnature AMC SeG */

typedef struct AMCSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  int *segTypeP;            /* .segtype */
  Bool new;                 /* allocated since last GC */
  Sig sig;                  /* impl.h.misc.sig */
} AMCSegStruct;

#define SegAMCSeg(seg)             ((AMCSeg)(seg))
#define AMCSegSeg(amcseg)          ((Seg)(amcseg))

#define AMCSegTypeP(seg)           (SegAMCSeg(seg)->segTypeP)
#define AMCSegSetTypeP(seg, type)  (SegAMCSeg(seg)->segTypeP = (type))


static Bool AMCSegCheck(AMCSeg amcseg)
{
  CHECKS(AMCSeg, amcseg);
  CHECKD(GCSeg, &amcseg->gcSegStruct);
  CHECKL(*amcseg->segTypeP == AMCPTypeNailBoard
         || *amcseg->segTypeP == AMCPTypeGen);
  CHECKL(BoolCheck(amcseg->new));
  return TRUE;
}


/* AMCSegInit -- initialise an AMC segment */

static Res AMCSegInit(Seg seg, Pool pool, Addr base, Size size, 
                      Bool reservoirPermit, va_list args)
{
  int *segtype = va_arg(args, int*);  /* .segtype */
  SegClass super;
  AMCSeg amcseg;
  Res res;

  AVERT(Seg, seg);
  amcseg = SegAMCSeg(seg);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(AMCSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;

  amcseg->segTypeP = segtype; /* .segtype */
  amcseg->new = TRUE;
  amcseg->sig = AMCSegSig;
  AVERT(AMCSeg, amcseg);

  return ResOK;
}


/* AMCSegDescribe -- describe the contents of a segment
 *
 * See design.mps.poolamc.seg-describe.
 */

static Res AMCSegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  Pool pool;
  AMCSeg amcseg;
  SegClass super;
  Addr i, p, base, limit, init;
  Align step;
  Size row;

  if (!CHECKT(Seg, seg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  amcseg = SegAMCSeg(seg);
  if (!CHECKT(AMCSeg, amcseg)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(GCSegClass);
  res = super->describe(seg, stream);
  if (res != ResOK) return res;

  pool = SegPool(seg);
  step = PoolAlignment(pool);
  row = step * 64;

  base = SegBase(seg);
  p = AddrAdd(base, pool->format->headerSize);
  limit = SegLimit(seg);
  if (SegBuffer(seg) != NULL)
    init = BufferGetInit(SegBuffer(seg));
  else
    init = limit;

  res = WriteF(stream,
               "AMC seg $P [$A,$A){\n",
               (WriteFP)seg, (WriteFA)base, (WriteFA)limit,
               "  Map\n",
               NULL);
  if (res != ResOK) return res;

  for(i = base; i < limit; i = AddrAdd(i, row)) {
    Addr j;
    char c;

    res = WriteF(stream, "    $A  ", i, NULL);
    if (res != ResOK) return res;

    /* @@@@ This needs to describe nailboards as well */
    /* @@@@ This misses a header-sized pad at the end. */
    for(j = i; j < AddrAdd(i, row); j = AddrAdd(j, step)) {
      if (j >= limit)
        c = ' ';
      else if (j >= init)
        c = '.';
      else if (j == p) {
        c = '*';
        p = (pool->format->skip)(p);
      } else
        c = '=';
      res = WriteF(stream, "$C", c, NULL);
      if (res != ResOK) return res;
    }

    res = WriteF(stream, "\n", NULL);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, "} AMC Seg $P\n", (WriteFP)seg, NULL);
  if (res != ResOK) return res;

  return ResOK;
}


/* AMCSegClass -- Class definition for AMC segments */

DEFINE_SEG_CLASS(AMCSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this (yet) */
  class->name = "AMCSEG";
  class->size = sizeof(AMCSegStruct);
  class->init = AMCSegInit;
  class->describe = AMCSegDescribe;
}



/* AMCSegHasNailBoard -- test whether the segment has a nail board
 *
 * See design.mps.poolamc.fix.nail.distinguish.
 */

static Bool AMCSegHasNailBoard(Seg seg)
{
  int type;

  type = *AMCSegTypeP(seg);
  AVER(type == AMCPTypeNailBoard || type == AMCPTypeGen);
  return type == AMCPTypeNailBoard;
}


/* AMCSegNailBoard -- get the nail board for this segment */

static AMCNailBoard AMCSegNailBoard(Seg seg)
{
  int *p;

  p = AMCSegTypeP(seg);
  AVER(AMCSegHasNailBoard(seg));
  return PARENT(AMCNailBoardStruct, type, p);
}


/* AMCSegGen -- get the generation structure for this segment */

static AMCGen AMCSegGen(Seg seg)
{
  if (AMCSegHasNailBoard(seg)) {
    AMCNailBoard nailBoard = AMCSegNailBoard(seg);
    return nailBoard->gen;
  } else {
    int *p;
    p = AMCSegTypeP(seg);
    return PARENT(AMCGenStruct, type, p);
  }
}


/* AMCStruct -- pool AMC descriptor
 *
 * See design.mps.poolamc.struct.
 */

#define AMCSig          ((Sig)0x519A3C99) /* SIGnature AMC */

typedef struct AMCStruct {      /* design.mps.poolamc.struct */
  PoolStruct poolStruct;        /* generic pool structure */
  RankSet rankSet;              /* rankSet for entire pool */
  RingStruct genRing;           /* ring of generations */
  Bool gensBooted;              /* used during boot (init) */
  Chain chain;                  /* chain used by this pool */
  size_t gens;                  /* number of generations */
  AMCGen *gen;                  /* (pointer to) array of generations */
  AMCGen nursery;               /* the default mutator generation */
  AMCGen rampGen;               /* the ramp generation */
  AMCGen afterRampGen;          /* the generation after rampGen */
  unsigned rampCount;           /* design.mps.poolamc.ramp.count */
  int rampMode;                 /* design.mps.poolamc.ramp.mode */
  Bool collectAll;              /* full collection after ramp? */
  Bool collectAllNext;          /* full collection after next ramp? */
  Sig sig;                      /* design.mps.pool.outer-structure.sig */
} AMCStruct;


/* PoolPoolAMC -- convert generic Pool to AMC */

#define PoolPoolAMC(pool) PARENT(AMCStruct, poolStruct, (pool))


/* AMCPool -- convert AMC to generic Pool */

#define AMCPool(amc) (&(amc)->poolStruct)


/* AMCGenCheck -- check consistency of a generation structure */

static Bool AMCGenCheck(AMCGen gen)
{
  Arena arena;
  AMC amc;

  CHECKS(AMCGen, gen);
  CHECKD(PoolGen, &gen->pgen);
  amc = AMCGenAMC(gen);
  CHECKU(AMC, amc);
  CHECKL(gen->type == AMCPTypeGen);
  CHECKD(Buffer, gen->forward);
  CHECKL(RingCheck(&gen->amcRing));
  CHECKL((gen->pgen.totalSize == 0) == (gen->segs == 0));
  arena = amc->poolStruct.arena;
  CHECKL(gen->pgen.totalSize >= gen->segs * ArenaAlign(arena));
  return TRUE;
}


/* AMCNailBoardCheck -- check the nail board */

static Bool AMCNailBoardCheck(AMCNailBoard board)
{
  CHECKS(AMCNailBoard, board);
  CHECKL(board->type == AMCPTypeNailBoard);
  CHECKD(AMCGen, board->gen);
  /* nails is >= number of set bits in mark, but we can't check this. */
  /* We know that shift corresponds to pool->align */
  CHECKL(BoolCheck(board->newMarks));
  CHECKL(board->distinctNails <= board->nails);
  CHECKL(1uL << board->markShift == PoolAlignment(AMCGenPool(board->gen)));
  /* weak check for BTs @@@@ */
  CHECKL(board->mark != NULL);
  return TRUE;
}


/* AMCBufStruct -- AMC Buffer subclass
 *
 * This subclass of SegBuf records a link to a generation.
 */

#define AMCBufSig ((Sig)0x519A3CBF) /* SIGnature AMC BuFfer  */ 

typedef struct AMCBufStruct *AMCBuf;

typedef struct AMCBufStruct {
  SegBufStruct segbufStruct;      /* superclass fields must come first */
  AMCGen gen;                     /* The AMC generation */
  Sig sig;                        /* design.mps.sig */
} AMCBufStruct;


/* BufferAMCBuf -- convert generic Buffer to an AMCBuf */

#define BufferAMCBuf(buffer) ((AMCBuf)(buffer))



/* AMCBufCheck -- check consistency of an AMCBuf */

static Bool AMCBufCheck(AMCBuf amcbuf)
{
  SegBuf segbuf;

  CHECKS(AMCBuf, amcbuf);
  segbuf = &amcbuf->segbufStruct;
  CHECKL(SegBufCheck(segbuf));
  if (amcbuf->gen != NULL)
    CHECKD(AMCGen, amcbuf->gen);
  return TRUE;
}


/* AMCBufGen -- Return the AMC generation of an AMCBuf */

static AMCGen AMCBufGen(Buffer buffer)
{
  AMCBuf amcbuf;
  AVERT(Buffer, buffer);
  amcbuf = BufferAMCBuf(buffer);
  AVERT(AMCBuf, amcbuf);
  return amcbuf->gen;
}


/* AMCBufSetGen -- Set the AMC generation of an AMCBuf */

static void AMCBufSetGen(Buffer buffer, AMCGen gen)
{
  AMCBuf amcbuf;

  AVERT(Buffer, buffer);
  if (gen != NULL)
    AVERT(AMCGen, gen);
  amcbuf = BufferAMCBuf(buffer);
  AVERT(AMCBuf, amcbuf);
  amcbuf->gen = gen;
}


/* AMCBufInit -- Initialize an AMCBuf */

static Res AMCBufInit(Buffer buffer, Pool pool, va_list args)
{
  AMC amc;
  AMCBuf amcbuf;
  BufferClass superclass;
  Res res;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);

  /* call next method */
  superclass = BUFFER_SUPERCLASS(AMCBufClass);
  res = (*superclass->init)(buffer, pool, args);
  if (res != ResOK)
    return res;

  amcbuf = BufferAMCBuf(buffer);
  if (BufferIsMutator(buffer)) {
    /* Set up the buffer to be allocating in the nursery. */
    amcbuf->gen = amc->nursery;
  } else {
    amcbuf->gen = NULL; /* no gen yet -- see design.mps.poolamc.forward.gen */
  }
  amcbuf->sig = AMCBufSig;
  AVERT(AMCBuf, amcbuf);

  BufferSetRankSet(buffer, amc->rankSet);

  return ResOK;
}


/* AMCBufFinish -- Finish an AMCBuf */

static void AMCBufFinish(Buffer buffer)
{
  BufferClass super;
  AMCBuf amcbuf;

  AVERT(Buffer, buffer);
  amcbuf = BufferAMCBuf(buffer);
  AVERT(AMCBuf, amcbuf);

  amcbuf->sig = SigInvalid;

  /* finish the superclass fields last */
  super = BUFFER_SUPERCLASS(AMCBufClass);
  super->finish(buffer);
}


/* AMCBufClass -- The class definition */

DEFINE_BUFFER_CLASS(AMCBufClass, class)
{
  INHERIT_CLASS(class, SegBufClass);
  class->name = "AMCBUF";
  class->size = sizeof(AMCBufStruct);
  class->init = AMCBufInit;
  class->finish = AMCBufFinish;
}


/* AMCGenCreate -- create a generation */

static Res AMCGenCreate(AMCGen *genReturn, AMC amc, Serial genNr)
{
  Arena arena;
  Buffer buffer;
  Pool pool;
  AMCGen gen;
  Res res;
  void *p;

  pool = AMCPool(amc);
  arena = pool->arena;

  res = ControlAlloc(&p, arena, sizeof(AMCGenStruct), FALSE);
  if (res != ResOK)
    goto failControlAlloc;
  gen = (AMCGen)p;

  res = BufferCreate(&buffer, EnsureAMCBufClass(), pool, FALSE);
  if (res != ResOK)
    goto failBufferCreate;

  res = PoolGenInit(&gen->pgen, amc->chain, genNr, pool);
  if (res != ResOK)
    goto failGenInit;
  gen->type = AMCPTypeGen;
  RingInit(&gen->amcRing);
  gen->segs = 0;
  gen->forward = buffer;
  gen->sig = AMCGenSig;

  AVERT(AMCGen, gen);

  RingAppend(&amc->genRing, &gen->amcRing);
  EVENT_PP(AMCGenCreate, amc, gen);
  *genReturn = gen;
  return ResOK;

failGenInit:
  BufferDestroy(buffer);
failBufferCreate:
  ControlFree(arena, p, sizeof(AMCGenStruct));
failControlAlloc:
  return res;
}


/* AMCGenDestroy -- destroy a generation */

static void AMCGenDestroy(AMCGen gen)
{
  Arena arena;

  AVERT(AMCGen, gen);
  AVER(gen->segs == 0);
  AVER(gen->pgen.totalSize == 0);

  EVENT_P(AMCGenDestroy, gen);
  arena = PoolArena(AMCPool(AMCGenAMC(gen)));
  gen->sig = SigInvalid;
  RingRemove(&gen->amcRing);
  RingFinish(&gen->amcRing);
  PoolGenFinish(&gen->pgen);
  BufferDestroy(gen->forward);
  ControlFree(arena, gen, sizeof(AMCGenStruct));
}


/* AMCGenDescribe -- describe an AMC generation */

static Res AMCGenDescribe(AMCGen gen, mps_lib_FILE *stream)
{
  Res res;

  if (!CHECKT(AMCGen, gen)) return ResFAIL;

  res = WriteF(stream,
               "  AMCGen $P ($U) {\n", (WriteFP)gen, (WriteFU)AMCGenNr(gen),
               "   buffer $P\n", gen->forward,
               "   segs $U, totalSize $U, newSize $U\n", (WriteFU)gen->segs,
               (WriteFU)gen->pgen.totalSize, (WriteFU)gen->pgen.newSize,
               "  } AMCGen\n", NULL);
  return res;
}


/* AMCSegCreateNailBoard -- create nail board for segment */

static Res AMCSegCreateNailBoard(Seg seg, Pool pool)
{
  AMCNailBoard board;
  Arena arena;
  Count bits;
  Res res;
  void *p;

  AVER(!AMCSegHasNailBoard(seg));

  arena = PoolArena(pool);

  res = ControlAlloc(&p, arena, sizeof(AMCNailBoardStruct), FALSE);
  if (res != ResOK)
    goto failAllocNailBoard;
  board = p;
  board->type = AMCPTypeNailBoard;
  board->gen = AMCSegGen(seg);
  board->nails = (Count)0;
  board->distinctNails = (Count)0;
  board->newMarks = FALSE;
  board->markShift = SizeLog2((Size)pool->alignment);
  /* See d.m.p.nailboard.size. */
  bits = (SegSize(seg) + pool->format->headerSize) >> board->markShift;
  res = ControlAlloc(&p, arena, BTSize(bits), FALSE);
  if (res != ResOK)
    goto failMarkTable;
  board->mark = p;
  BTResRange(board->mark, 0, bits);
  board->sig = AMCNailBoardSig;
  AVERT(AMCNailBoard, board);
  AMCSegSetTypeP(seg, &board->type); /* .segtype */
  return ResOK;

failMarkTable:
  ControlFree(arena, board, sizeof(AMCNailBoardStruct));
failAllocNailBoard:
  return res;
}


/* AMCSegDestroyNailBoard -- destroy the nail board of a segment */

static void AMCSegDestroyNailBoard(Seg seg, Pool pool)
{
  AMCNailBoard board;
  AMCGen gen;
  Arena arena;
  Count bits;

  gen = AMCSegGen(seg);
  AVERT(AMCGen, gen);
  board = AMCSegNailBoard(seg);
  AVERT(AMCNailBoard, board);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  bits = SegSize(seg) >> board->markShift;
  ControlFree(arena, board->mark, BTSize(bits));
  board->sig = SigInvalid;
  ControlFree(arena, board, sizeof(AMCNailBoardStruct));
  AMCSegSetTypeP(seg, &gen->type); /* .segtype */
}


/* AMCNailGetMark -- get the mark bit for ref from the nail board */

static Bool AMCNailGetMark(Seg seg, Ref ref)
{
  AMCNailBoard board;
  Index i;

  board = AMCSegNailBoard(seg);
  AVERT(AMCNailBoard, board);

  i = AddrOffset(SegBase(seg), ref) >> board->markShift;
  return BTGet(board->mark, i);
}


/* AMCNailGetAndSetMark -- set the mark bit for ref in the nail board
 *
 * Returns the old value.
 */

static Bool AMCNailGetAndSetMark(Seg seg, Ref ref)
{
  AMCNailBoard board;
  Index i;

  board = AMCSegNailBoard(seg);
  AVERT(AMCNailBoard, board);

  ++board->nails;
  i = AddrOffset(SegBase(seg), ref) >> board->markShift;
  if (!BTGet(board->mark, i)) {
    BTSet(board->mark, i);
    board->newMarks = TRUE;
    ++board->distinctNails;
    return FALSE;
  }
  return TRUE;
}


/* AMCNailMarkRange -- nail a range in the board
 *
 * We nail the objects laying between base and limit, i.e., mark the
 * bits that correspond to client pointers for them.  We may assume that
 * the range is unmarked.
 */

static void AMCNailMarkRange(Seg seg, Addr base, Addr limit)
{
  AMCNailBoard board;
  Index ibase, ilimit;
  Size headerSize;

  AVER(SegBase(seg) <= base && base < SegLimit(seg));
  AVER(SegBase(seg) <= limit && limit <= SegLimit(seg));
  AVER(base < limit);

  board = AMCSegNailBoard(seg);
  AVERT(AMCNailBoard, board);
  headerSize = SegPool(seg)->format->headerSize;
  ibase = (AddrOffset(SegBase(seg), base) + headerSize) >> board->markShift;
  ilimit = (AddrOffset(SegBase(seg), limit) + headerSize) >> board->markShift;
  AVER(BTIsResRange(board->mark, ibase, ilimit));

  BTSetRange(board->mark, ibase, ilimit);
  board->nails += ilimit - ibase;
  board->distinctNails += ilimit - ibase;
}


/* AMCNailRangeIsMarked -- check that a range in the board is marked
 *
 * Like AMCNailMarkRange, we take the arguments as referring to base
 * pointers and look at the bits of the corresponding client pointers.
 */

static Bool AMCNailRangeIsMarked(Seg seg, Addr base, Addr limit)
{
  AMCNailBoard board;
  Index ibase, ilimit;
  Size headerSize;

  AVER(SegBase(seg) <= base && base < SegLimit(seg));
  AVER(SegBase(seg) <= limit && limit <= SegLimit(seg));
  AVER(base < limit);

  board = AMCSegNailBoard(seg);
  AVERT(AMCNailBoard, board);
  headerSize = SegPool(seg)->format->headerSize;
  ibase = (AddrOffset(SegBase(seg), base) + headerSize) >> board->markShift;
  ilimit = (AddrOffset(SegBase(seg), limit) + headerSize) >> board->markShift;
  return BTIsSetRange(board->mark, ibase, ilimit);
}


/* AMCInitComm -- initialize AMC/Z pool
 *
 * See design.mps.poolamc.init.
 * Shared by AMCInit and AMCZinit.
 */

static Res AMCInitComm(Pool pool, RankSet rankSet, va_list arg)
{
  AMC amc;
  Res res;
  Arena arena;
  Index i;
  Size genArraySize;
  size_t genCount;

  AVER(pool != NULL);

  amc = PoolPoolAMC(pool);
  arena = PoolArena(pool);

  pool->format = va_arg(arg, Format);
  AVERT(Format, pool->format);
  pool->alignment = pool->format->alignment;
  amc->chain = va_arg(arg, Chain);
  AVERT(Chain, amc->chain);
  amc->rankSet = rankSet;

  RingInit(&amc->genRing);
  /* amc gets checked before the generations get created, but they */
  /* do get created later in this function. */
  amc->gen = NULL;
  amc->nursery = NULL;
  amc->rampGen = NULL;
  amc->afterRampGen = NULL;
  amc->gensBooted = FALSE;

  amc->rampCount = 0;
  amc->rampMode = outsideRamp;
  amc->collectAll = FALSE; amc->collectAllNext = FALSE;

  if (pool->format->headerSize == 0) {
    pool->fix = AMCFix;
  } else {
    pool->fix = AMCHeaderFix;
  }

  amc->sig = AMCSig;
  AVERT(AMC, amc);

  /* Init generations. */
  genCount = ChainGens(amc->chain);
  {
    void *p;

    genArraySize = sizeof(AMCGen) * (genCount + 1); /* chain plus dynamic gen */
    res = ControlAlloc(&p, arena, genArraySize, FALSE);
    if (res != ResOK)
      goto failGensAlloc;
    amc->gen = p;
    for(i = 0; i < genCount + 1; ++i) {
      res = AMCGenCreate(&amc->gen[i], amc, (Serial)i);
      if (res != ResOK) {
        goto failGenAlloc;
      }
    }
    /* Set up forwarding buffers. */
    for(i = 0; i < genCount; ++i) {
      AMCBufSetGen(amc->gen[i]->forward, amc->gen[i+1]);
    }
    /* Dynamic gen forwards to itself. */
    AMCBufSetGen(amc->gen[genCount]->forward, amc->gen[genCount]);
  }
  amc->nursery = amc->gen[0];
  amc->rampGen = amc->gen[genCount-1]; /* last ephemeral gen */
  amc->afterRampGen = amc->gen[genCount];
  amc->gensBooted = TRUE;

  AVERT(AMC, amc);
  EVENT_PP(AMCInit, pool, amc);
  if (rankSet == RankSetEMPTY)
    EVENT_PP(PoolInitAMCZ, pool, pool->format);
  else
    EVENT_PP(PoolInitAMC, pool, pool->format);
  return ResOK;

failGenAlloc:
  while(i > 0) {
    --i;
    AMCGenDestroy(amc->gen[i]);
  }
  ControlFree(arena, amc->gen, genArraySize);
failGensAlloc:
  return res;
}

static Res AMCInit(Pool pool, va_list arg)
{
  return AMCInitComm(pool, RankSetSingle(RankEXACT), arg);
}

static Res AMCZInit(Pool pool, va_list arg)
{
  return AMCInitComm(pool, RankSetEMPTY, arg);
}


/* AMCFinish -- finish AMC pool
 *
 * See design.mps.poolamc.finish.
 */

static void AMCFinish(Pool pool)
{
  AMC amc;
  Ring ring;
  Ring node, nextNode;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);

  EVENT_P(AMCFinish, amc);

  /* @@@@ Make sure that segments aren't buffered by forwarding buffers. */
  /* This is a hack which allows the pool to be destroyed */
  /* while it is collecting.  Note that there aren't any mutator */
  /* buffers by this time. */
  RING_FOR(node, &amc->genRing, nextNode) {
    AMCGen gen = RING_ELT(AMCGen, amcRing, node);
    BufferDetach(gen->forward, pool);
    gen->pgen.newSize = (Size)0; /* to maintain invariant < totalSize */
  }

  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    Size size;
    AMCGen gen = AMCSegGen(seg);

    --gen->segs;
    size = SegSize(seg);
    gen->pgen.totalSize -= size;

    SegFree(seg);
  }

  /* Disassociate forwarding buffers from gens before they are destroyed */
  ring = &amc->genRing;
  RING_FOR(node, ring, nextNode) {
    AMCGen gen = RING_ELT(AMCGen, amcRing, node);
    AMCBufSetGen(gen->forward, NULL);
  }
  RING_FOR(node, ring, nextNode) {
    AMCGen gen = RING_ELT(AMCGen, amcRing, node);
    AMCGenDestroy(gen);
  }

  amc->sig = SigInvalid;
}


/* AMCBufferFill -- refill an allocation buffer
 *
 * See design.mps.poolamc.fill.
 */

static Res AMCBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size,
                         Bool withReservoirPermit)
{
  Seg seg;
  AMC amc;
  Res res;
  Addr base, limit;
  Arena arena;
  Size alignedSize;
  AMCGen gen;
  Serial genNr;
  SegPrefStruct segPrefStruct;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size >  0);
  AVER(BoolCheck(withReservoirPermit));

  gen = AMCBufGen(buffer);
  AVERT(AMCGen, gen);

  /* Create and attach segment.  The location of this segment is */
  /* expressed as a generation number.  We rely on the arena to */
  /* organize locations appropriately.  */
  arena = PoolArena(pool);
  alignedSize = SizeAlignUp(size, ArenaAlign(arena));
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  genNr = PoolGenNr(&gen->pgen);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &genNr);
  res = SegAlloc(&seg, EnsureAMCSegClass(), &segPrefStruct,
                 alignedSize, pool, withReservoirPermit,
                 &gen->type); /* .segtype */
  if (res != ResOK)
    return res;

  /* design.mps.seg.field.rankSet.start */
  if (BufferRankSet(buffer) == RankSetEMPTY) {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  } else {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);
  }

  /* Put the segment in the generation indicated by the buffer. */
  ++gen->segs;
  gen->pgen.totalSize += alignedSize;
  /* If ramping, don't count survivors in newSize. */
  if (amc->rampMode != ramping
      || buffer != amc->rampGen->forward || gen != amc->rampGen) {
    gen->pgen.newSize += alignedSize;
  } else {
    SegAMCSeg(seg)->new = FALSE;
  }
  PoolGenUpdateZones(&gen->pgen, seg);

  /* Give the buffer the entire segment to allocate in. */
  base = SegBase(seg);
  *baseReturn = base;
  limit = AddrAdd(base, alignedSize);
  AVER(limit == SegLimit(seg));
  *limitReturn = limit;
  return ResOK;
}


/* AMCBufferEmpty -- detach a buffer from a segment
 *
 * See design.mps.poolamc.flush.
 */

static void AMCBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  AMC amc;
  Word size;
  Arena arena;
  Seg seg;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);
  AVER(SegLimit(seg) == limit);

  arena = BufferArena(buffer);

  /* design.mps.poolamc.flush.pad */
  size = AddrOffset(init, limit);
  if (size > 0) {
    ShieldExpose(arena, seg);
    (*pool->format->pad)(init, size);
    ShieldCover(arena, seg);
  }
}


/* AMCRampBegin -- note an entry into a ramp pattern */

static void AMCRampBegin(Pool pool, Buffer buf, Bool collectAll)
{
  AMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  AVERT(Buffer, buf);
  AVERT(Bool, collectAll);

  AVER(amc->rampCount < UINT_MAX);
  ++amc->rampCount;
  if (amc->rampCount == 1) {
    amc->collectAllNext = FALSE;
    if (amc->rampMode != finishRamp)
      amc->rampMode = beginRamp;
  }
  amc->collectAllNext |= collectAll;
}


/* AMCRampEnd -- note an exit from a ramp pattern */

static void AMCRampEnd(Pool pool, Buffer buf)
{
  AMC amc;

  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  AVERT(Buffer, buf);

  AVER(amc->rampCount > 0);
  --amc->rampCount;
  if (amc->rampCount == 0) {
    PoolGen pgen = &amc->rampGen->pgen;
    Ring node, nextNode;

    if (amc->rampMode == ramping) { /* if we are ramping, clean up */
      amc->rampMode = finishRamp;
      amc->collectAll = amc->collectAllNext;
    } else
      amc->rampMode = outsideRamp;

    /* Adjust amc->rampGen->pgen.newSize: Now count all the segments in the */
    /* ramp generation as new (except if they're white). */
    RING_FOR(node, PoolSegRing(pool), nextNode) {
      Seg seg = SegOfPoolRing(node);

      if (AMCSegGen(seg) == amc->rampGen && !SegAMCSeg(seg)->new
          && SegWhite(seg) == TraceSetEMPTY) {
        pgen->newSize += SegSize(seg);
        SegAMCSeg(seg)->new = TRUE;
      }
    }
  }
}


/* AMCWhiten -- condemn the segment for the trace
 *
 * If the segment has a mutator buffer on it, we nail the buffer,
 * because we can't scan or reclaim uncommitted buffers.
 */

static Res AMCWhiten(Pool pool, Trace trace, Seg seg)
{
  AMCGen gen;
  AMC amc;
  Buffer buffer;
  Res res;

  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  buffer = SegBuffer(seg);
  if (buffer != NULL) {
    AVERT(Buffer, buffer);

    if (!BufferIsMutator(buffer)) {   /* forwarding buffer */
      AVER(BufferIsReady(buffer));
      BufferDetach(buffer, pool);
    } else {                        /* mutator buffer */
      if (BufferScanLimit(buffer) == SegBase(seg))
        /* There's nothing but the buffer, don't condemn. */
        return ResOK;
      else /* if (BufferScanLimit(buffer) == BufferLimit(buffer)) { */
        /* The buffer is full, so it won't be used by the mutator. */
        /* @@@@ We should detach it, but can't for technical reasons. */
        /* BufferDetach(buffer, pool); */
      /* } else */ {
        /* There is an active buffer, make sure it's nailed. */
        if (!AMCSegHasNailBoard(seg)) {
          if (SegNailed(seg) == TraceSetEMPTY) {
            res = AMCSegCreateNailBoard(seg, pool);
            if (res != ResOK)
              return ResOK; /* can't create nail board, don't condemn */
            if (BufferScanLimit(buffer) != BufferLimit(buffer))
              AMCNailMarkRange(seg, BufferScanLimit(buffer),
                               BufferLimit(buffer));
            ++trace->nailCount;
            SegSetNailed(seg, TraceSetSingle(trace));
          } else {
            /* Segment is nailed already, cannot create a nail board */
            /* (see .nail.new), just give up condemning. */
            return ResOK;
          }
        } else {
          /* We have a nail board, the buffer must be nailed already. */
          AVER((BufferScanLimit(buffer) == BufferLimit(buffer))
               || AMCNailRangeIsMarked(seg, BufferScanLimit(buffer),
                                       BufferLimit(buffer)));
          /* Nail it for this trace as well. */
          SegSetNailed(seg, TraceSetAdd(SegNailed(seg), trace));
        }
        /* We didn't condemn the buffer, subtract it from the count. */
        /* @@@@ We could subtract all the nailed grains. */
	/* Relies on unsigned arithmetic wrapping round */
	/* on under- and overflow (which it does) and on */
	/* trace->condemned being unsigned. */
        trace->condemned -= AddrOffset(BufferScanLimit(buffer),
                                       BufferLimit(buffer));
      }
    }
  }

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  trace->condemned += SegSize(seg);

  gen = AMCSegGen(seg);
  AVERT(AMCGen, gen);
  if (SegAMCSeg(seg)->new) {
    gen->pgen.newSize -= SegSize(seg);
    SegAMCSeg(seg)->new = FALSE;
  }

  /* Ensure we are forwarding into the right generation. */

  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  /* see design.mps.poolamc.gen.ramp */
  /* This switching needs to be more complex for multiple traces. */
  AVER(TraceSetIsSingle(PoolArena(pool)->busyTraces));
  if (amc->rampMode == beginRamp && gen == amc->rampGen) {
    BufferDetach(gen->forward, pool);
    AMCBufSetGen(gen->forward, gen);
    amc->rampMode = ramping;
  } else {
    if (amc->rampMode == finishRamp && gen == amc->rampGen) {
      BufferDetach(gen->forward, pool);
      AMCBufSetGen(gen->forward, amc->afterRampGen);
      amc->rampMode = collectingRamp;
    }
  }

  return ResOK;
}


/* AMCScanNailedOnce -- make one scanning pass over a nailed segment
 *
 * *totalReturn set to TRUE iff all objects in segment scanned.
 * *moreReturn set to FALSE only if there are no more objects
 * on the segment that need scanning (which is normally the case).
 * It is set to TRUE if scanning had to be abandoned early on, and
 * also if during emergency fixing any new marks got added to the
 * nail board.
 */

static Res AMCScanNailedOnce(Bool *totalReturn, Bool *moreReturn,
                             ScanState ss, Pool pool,
                             Seg seg, AMC amc)
{
  Addr p, limit;
  Format format;
  Res res;
  Bool total = TRUE;
  Size bytesScanned = 0;

  UNUSED(amc); /* Actually only unused when telemetry is off. @@@@ */

  EVENT_PPP(AMCScanBegin, amc, seg, ss); /* @@@@ use own event */

  format = pool->format;
  AMCSegNailBoard(seg)->newMarks = FALSE;

  p = AddrAdd(SegBase(seg), format->headerSize);
  while(SegBuffer(seg) != NULL) {
    limit = AddrAdd(BufferScanLimit(SegBuffer(seg)), format->headerSize);
    if (p >= limit) {
      AVER(p == limit);
      goto returnGood;
    }
    while(p < limit) {
      Addr q;
      q = (*format->skip)(p);
      if (AMCNailGetMark(seg, p)) {
        res = (*format->scan)(ss, p, q);
        if (res != ResOK) {
          *totalReturn = FALSE; *moreReturn = TRUE;
          return res;
        }
        bytesScanned += AddrOffset(p, q);
      } else {
        total = FALSE;
      }
      p = q;
    }
    AVER(p == limit);
  }

  /* Should have a ScanMarkedRange or something like that @@@@ */
  /* to abstract common code. */

  limit = AddrAdd(SegLimit(seg), format->headerSize);
  /* @@@@ Shouldn't p be set to BufferLimit here?! */
  while(p < limit) {
    Addr q;
    q = (*format->skip)(p);
    if (AMCNailGetMark(seg, p)) {
      res = (*format->scan)(ss, p, q);
      if (res != ResOK) {
        *totalReturn = FALSE; *moreReturn = TRUE;
        return res;
      }
      bytesScanned += AddrOffset(p, q);
    } else {
      total = FALSE;
    }
    p = q;
  }
  AVER(p == limit);

returnGood:
  EVENT_PPP(AMCScanEnd, amc, seg, ss); /* @@@@ use own event */

  AVER(bytesScanned <= SegSize(seg));
  ss->scannedSize += bytesScanned;
  *totalReturn = total;
  *moreReturn = AMCSegNailBoard(seg)->newMarks;
  return ResOK;
}


/* AMCScanNailed -- scan a nailed segment */

static Res AMCScanNailed(Bool *totalReturn, ScanState ss, Pool pool,
                         Seg seg, AMC amc)
{
  Bool total;
  Bool moreScanning;

  do {
    Res res;
    res = AMCScanNailedOnce(&total, &moreScanning, ss, pool, seg, amc);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  } while(moreScanning);

  *totalReturn = total;
  return ResOK;
}


/* AMCScan -- scan a single seg, turning it black
 *
 * See design.mps.poolamc.seg-scan.
 */

static Res AMCScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Addr base, limit;
  Arena arena;
  Format format;
  AMC amc;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVERT(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);


  format = pool->format;
  arena = pool->arena;

  if (AMCSegHasNailBoard(seg)) {
    return AMCScanNailed(totalReturn, ss, pool, seg, amc);
  }

  EVENT_PPP(AMCScanBegin, amc, seg, ss);

  base = AddrAdd(SegBase(seg), format->headerSize);
  while(SegBuffer(seg) != NULL) {  /* design.mps.poolamc.seg-scan.loop */
    limit = AddrAdd(BufferScanLimit(SegBuffer(seg)), format->headerSize);
    if (base >= limit) {
      /* @@@@ Are we sure we don't need scan the rest of the segment? */
      AVER(base == limit);
      *totalReturn = TRUE;
      return ResOK;
    }
    res = (*format->scan)(ss, base, limit);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    ss->scannedSize += AddrOffset(base, limit);
    base = limit;
  }

  /* design.mps.poolamc.seg-scan.finish @@@@ base? */
  limit = AddrAdd(SegLimit(seg), format->headerSize);
  AVER(SegBase(seg) <= base
       && base <= AddrAdd(SegLimit(seg), format->headerSize));
  if (base < limit) {
    res = (*format->scan)(ss, base, limit);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  }

  ss->scannedSize += AddrOffset(base, limit);
  EVENT_PPP(AMCScanEnd, amc, seg, ss);

  *totalReturn = TRUE;
  return ResOK;
}


/* AMCFixInPlace -- fix an reference without moving the object
 *
 * Usually this function is used for ambiguous references, but during
 * emergency tracing may be used for references of any rank.
 *
 * If the segment has a nail board then we use that to record the fix.
 * Otherwise we simply grey and nail the entire segment.
 */

static void AMCFixInPlace(Pool pool, Seg seg, ScanState ss, Ref *refIO)
{
  Addr ref;

  /* arguments AVERed by AMCFix */
  UNUSED(pool);

  ref = (Addr)*refIO;
  /* An ambiguous reference can point before the header. */
  AVER(SegBase(seg) <= ref);
  /* .ref-limit: A reference passed to Fix can't be beyond the segment, */
  /* because then TraceFix would not have picked this segment. */
  AVER(ref < SegLimit(seg));

  EVENT_0(AMCFixInPlace);
  if (AMCSegHasNailBoard(seg)) {
    Bool wasMarked = AMCNailGetAndSetMark(seg, ref);
    /* If there are no new marks (i.e., no new traces for which we */
    /* are marking, and no new mark bits set) then we can return */
    /* immediately, without changing colour. */
    if (TraceSetSub(ss->traces, SegNailed(seg)) && wasMarked)
      return;
  } else if (TraceSetSub(ss->traces, SegNailed(seg))) {
    return;
  }
  SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
  if (SegRankSet(seg) != RankSetEMPTY)
    SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
}


/* AMCFixEmergency -- fix a reference, without allocating
 *
 * See design.mps.poolamc.emergency.fix.
 */

static Res AMCFixEmergency(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Addr newRef;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);

  ss->wasMarked = TRUE;

  if (ss->rank == RankAMBIG)
    goto fixInPlace;

  ShieldExpose(arena, seg);
  newRef = (*pool->format->isMoved)(*refIO);
  ShieldCover(arena, seg);
  if (newRef != (Addr)0) {
    /* Object has been forwarded already, so snap-out pointer. */
    /* Useful weak pointer semantics not implemented. @@@@ */
    *refIO = newRef;
    return ResOK;
  }

fixInPlace: /* see design.mps.poolamc.nailboard.emergency */
  AMCFixInPlace(pool, seg, ss, refIO);
  return ResOK;
}


/* AMCFix -- fix a reference to the pool
 *
 * See design.mps.poolamc.fix.
 */

Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Res res;
  Format format;        /* cache of pool->format */
  Ref ref;              /* reference to be fixed */
  Ref newRef;           /* new location, if moved */
  Size length;          /* length of object to be relocated */
  Buffer buffer;        /* buffer to allocate new copy into */
  AMCGen gen;           /* generation of old copy of object */
  TraceSet grey;        /* greyness of object being relocated */
  TraceSet toGrey;      /* greyness of object's destination */
  RefSet summary;       /* summary of object being relocated */
  RefSet toSummary;     /* summary of object's destination */
  Seg toSeg;            /* segment to which object is being relocated */
  Bool shieldUp;        /* whether we have exposed seg */

  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);
  EVENT_0(AMCFix);

  /* For the moment, assume that the object was already marked. */
  /* (See design.mps.fix.protocol.was-marked.) */
  ss->wasMarked = TRUE;

  /* If the reference is ambiguous, set up the datastructures for */
  /* managing a nailed segment.  This involves marking the segment */
  /* as nailed, and setting up a per-word mark table */
  if (ss->rank == RankAMBIG) {
    /* .nail.new: Check to see whether we need a NailBoard for */
    /* this seg.  We use "SegNailed(seg) == TraceSetEMPTY" */
    /* rather than "!AMCSegHasNailBoard(seg)" because this avoids */
    /* setting up a new nail board when the segment was nailed, but had */
    /* no nail board.  This must be avoided because otherwise */
    /* assumptions in AMCFixEmergency will be wrong (essentially */
    /* we will lose some pointer fixes because we introduced a */
    /* nail board). */
    if (SegNailed(seg) == TraceSetEMPTY) {
      res = AMCSegCreateNailBoard(seg, pool);
      if (res != ResOK)
        return res;
      ++ss->nailCount;
      SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
    }
    AMCFixInPlace(pool, seg, ss, refIO);
    return ResOK;
  }

  amc = PoolPoolAMC(pool);
  AVERT_CRITICAL(AMC, amc);
  format = pool->format;
  ref = *refIO;
  AVER_CRITICAL(SegBase(seg) <= ref);
  AVER_CRITICAL(ref < SegLimit(seg));
  arena = pool->arena;

  /* .access.read: Make sure seg isn't behind a read barrier. */
  shieldUp = FALSE;
  if (SegPM(seg) & AccessREAD) {
    ShieldExpose(arena, seg);
    shieldUp = TRUE;
  }
  newRef = (*format->isMoved)(ref);

  if (newRef == (Addr)0) {
    /* If object is nailed already then we mustn't copy it: */
    if (SegNailed(seg) != TraceSetEMPTY
        && (!AMCSegHasNailBoard(seg) || AMCNailGetMark(seg, ref))) {
      /* Segment only needs greying if there are new traces for which */
      /* we are nailing. */
      if (!TraceSetSub(ss->traces, SegNailed(seg))) {
        if (SegRankSet(seg) != RankSetEMPTY)
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
        SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
      }
      res = ResOK;
      goto returnRes;
    } else if (ss->rank == RankWEAK) {
      /* object is not preserved (neither moved, nor nailed) */
      /* hence, reference should be splatted */
      goto updateReference;
    }
    /* object is not preserved yet (neither moved, nor nailed) */
    /* so should be preserved by forwarding */
    EVENT_A(AMCFixForward, newRef);
    /* design.mps.fix.protocol.was-marked */
    ss->wasMarked = FALSE;

    /* Get the forwarding buffer from the object's generation. */
    gen = AMCSegGen(seg);
    buffer = gen->forward;
    AVER_CRITICAL(buffer != NULL);

    length = AddrOffset(ref, (*format->skip)(ref));
    STATISTIC_STAT(++ss->forwardedCount);
    ss->forwardedSize += length;
    do {
      res = BUFFER_RESERVE(&newRef, buffer, length, FALSE);
      if (res != ResOK)
        goto returnRes;

      toSeg = BufferSeg(buffer);
      ShieldExpose(arena, toSeg);

      /* Since we're moving an object from one segment to another, */
      /* union the greyness and the summaries together. */
      grey = TraceSetUnion(ss->traces, SegGrey(seg));
      toGrey = SegGrey(toSeg);
      if (TraceSetDiff(grey, toGrey) != TraceSetEMPTY
          && SegRankSet(seg) != RankSetEMPTY)
        SegSetGrey(toSeg, TraceSetUnion(toGrey, grey));
      summary = SegSummary(seg);
      toSummary = SegSummary(toSeg);
      if (RefSetDiff(summary, toSummary) != RefSetEMPTY)
        SegSetSummary(toSeg, RefSetUnion(toSummary, summary));

      /* design.mps.trace.fix.copy */
      (void)AddrCopy(newRef, ref, length);

      ShieldCover(arena, toSeg);
    } while (!BUFFER_COMMIT(buffer, newRef, length));
    ss->copiedSize += length;

    /* Make sure there's no read or write barrier. */
    if (!shieldUp && (SegPM(seg) & (AccessWRITE | AccessREAD))) {
      ShieldExpose(arena, seg);
      shieldUp = TRUE;
    }
    (*format->move)(ref, newRef);
  } else {
    /* reference to broken heart (which should be snapped out -- */
    /* consider adding to (non-existant) snap-out cache here) */
    STATISTIC_STAT(++ss->snapCount);
  }

  /* .fix.update: update the reference to whatever the above code */
  /* decided it should be */
updateReference:
  *refIO = newRef;
  res = ResOK;

returnRes:
  if (shieldUp)
    ShieldCover(arena, seg);
  return res;
}


/* AMCHeaderFix -- fix a reference to the pool, with headers
 *
 * See design.mps.poolamc.header.fix.
 */

static Res AMCHeaderFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Res res;
  Format format;        /* cache of pool->format */
  Ref ref;              /* reference to be fixed */
  Ref newRef;           /* new location, if moved */
  Addr newBase;         /* base address of new copy */
  Size length;          /* length of object to be relocated */
  Buffer buffer;        /* buffer to allocate new copy into */
  AMCGen gen;           /* generation of old copy of object */
  TraceSet grey;        /* greyness of object being relocated */
  TraceSet toGrey;      /* greyness of object's destination */
  RefSet summary;       /* summary of object being relocated */
  RefSet toSummary;     /* summary of object's destination */
  Seg toSeg;            /* segment to which object is being relocated */
  Bool shieldUp;        /* whether we have exposed seg */

  /* design.mps.trace.fix.noaver */
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);
  EVENT_0(AMCFix);

  /* For the moment, assume that the object was already marked. */
  /* (See design.mps.fix.protocol.was-marked.) */
  ss->wasMarked = TRUE;

  /* If the reference is ambiguous, set up the datastructures for */
  /* managing a nailed segment.  This involves marking the segment */
  /* as nailed, and setting up a per-word mark table */
  if (ss->rank == RankAMBIG) {
    /* .nail.new: Check to see whether we need a NailBoard for */
    /* this seg.  We use "SegNailed(seg) == TraceSetEMPTY" */
    /* rather than "!AMCSegHasNailBoard(seg)" because this avoids */
    /* setting up a new nail board when the segment was nailed, but had */
    /* no nail board.  This must be avoided because otherwise */
    /* assumptions in AMCFixEmergency will be wrong (essentially */
    /* we will lose some pointer fixes because we introduced a */
    /* nail board). */
    if (SegNailed(seg) == TraceSetEMPTY) {
      res = AMCSegCreateNailBoard(seg, pool);
      if (res != ResOK)
        return res;
      ++ss->nailCount;
      SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
    }
    AMCFixInPlace(pool, seg, ss, refIO);
    return ResOK;
  }

  amc = PoolPoolAMC(pool);
  AVERT_CRITICAL(AMC, amc);
  format = pool->format;
  ref = *refIO;
  AVER_CRITICAL(AddrAdd(SegBase(seg), pool->format->headerSize) <= ref);
  AVER_CRITICAL(ref < SegLimit(seg)); /* see .ref-limit */
  arena = pool->arena;

  /* .access.read.header: Make sure seg isn't behind a read barrier. */
  shieldUp = FALSE;
  if (SegPM(seg) & AccessREAD) {
    ShieldExpose(arena, seg);
    shieldUp = TRUE;
  }
  newRef = (*format->isMoved)(ref);

  if (newRef == (Addr)0) {
    /* If object is nailed already then we mustn't copy it: */
    if (SegNailed(seg) != TraceSetEMPTY
        && (!AMCSegHasNailBoard(seg) || AMCNailGetMark(seg, ref))) {
      /* Segment only needs greying if there are new traces for which */
      /* we are nailing. */
      if (!TraceSetSub(ss->traces, SegNailed(seg))) {
        if (SegRankSet(seg) != RankSetEMPTY)
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
        SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
      }
      res = ResOK;
      goto returnRes;
    } else if (ss->rank == RankWEAK) {
      /* object is not preserved (neither moved, nor nailed) */
      /* hence, reference should be splatted */
      goto updateReference;
    }
    /* object is not preserved yet (neither moved, nor nailed) */
    /* so should be preserved by forwarding */
    EVENT_A(AMCFixForward, newRef);
    /* design.mps.fix.protocol.was-marked */
    ss->wasMarked = FALSE;

    /* Get the forwarding buffer from the object's generation. */
    gen = AMCSegGen(seg);
    buffer = gen->forward;
    AVER_CRITICAL(buffer != NULL);

    length = AddrOffset(ref, (*format->skip)(ref));
    STATISTIC_STAT(++ss->forwardedCount);
    ss->forwardedSize += length;
    do {
      Size headerSize = format->headerSize;

      res = BUFFER_RESERVE(&newBase, buffer, length, FALSE);
      if (res != ResOK)
        goto returnRes;
      newRef = AddrAdd(newBase, headerSize);

      toSeg = BufferSeg(buffer);
      ShieldExpose(arena, toSeg);

      /* Since we're moving an object from one segment to another, */
      /* union the greyness and the summaries together. */
      grey = TraceSetUnion(ss->traces, SegGrey(seg));
      toGrey = SegGrey(toSeg);
      if (TraceSetDiff(grey, toGrey) != TraceSetEMPTY
          && SegRankSet(seg) != RankSetEMPTY)
        SegSetGrey(toSeg, TraceSetUnion(toGrey, grey));
      summary = SegSummary(seg);
      toSummary = SegSummary(toSeg);
      if (RefSetDiff(summary, toSummary) != RefSetEMPTY)
        SegSetSummary(toSeg, RefSetUnion(toSummary, summary));

      /* design.mps.trace.fix.copy */
      (void)AddrCopy(newBase, AddrSub(ref, headerSize), length);

      ShieldCover(arena, toSeg);
    } while (!BUFFER_COMMIT(buffer, newBase, length));
    ss->copiedSize += length;

    /* Make sure there's no read or write barrier. */
    if (!shieldUp && (SegPM(seg) & (AccessWRITE | AccessREAD))) {
      ShieldExpose(arena, seg);
      shieldUp = TRUE;
    }
    (*format->move)(ref, newRef);
  } else {
    /* reference to broken heart (which should be snapped out -- */
    /* consider adding to (non-existent) snap-out cache here) */
    STATISTIC_STAT(++ss->snapCount);
  }

  /* .fix.update: update the reference to whatever the above code */
  /* decided it should be */
updateReference:
  *refIO = newRef;
  res = ResOK;

returnRes:
  if (shieldUp)
    ShieldCover(arena, seg);
  return res;
}


/* AMCReclaimNailed -- reclaim what you can from a nailed segment */

static void AMCReclaimNailed(Pool pool, Trace trace, Seg seg)
{
  Addr p, limit;
  Arena arena;
  Format format;
  Size bytesReclaimed = (Size)0;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  AMC amc;
  Size headerSize;

  /* All arguments AVERed by AMCReclaim */

  amc = PoolPoolAMC(pool);
  AVERT(AMC, amc);
  format = pool->format;

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  if (!AMCSegHasNailBoard(seg)) {
    /* We didn't keep a mark table, so preserve everything. */
    /* We can't do anything about preservedInPlaceCount. */
    trace->preservedInPlaceSize += SegSize(seg);
    goto adjustColour;
  }

  /* see design.mps.poolamc.nailboard.limitations for improvements */
  headerSize = format->headerSize;
  ShieldExpose(arena, seg);
  p = AddrAdd(SegBase(seg), headerSize);
  if (SegBuffer(seg) != NULL)
    limit = BufferScanLimit(SegBuffer(seg));
  else
    limit = SegLimit(seg);
  limit = AddrAdd(limit, headerSize);
  while(p < limit) {
    Addr q;
    Size length;
    q = (*format->skip)(p);
    length = AddrOffset(p, q);
    if (!AMCNailGetMark(seg, p)) {
      (*format->pad)(AddrSub(p, headerSize), length);
      bytesReclaimed += length;
    } else {
      ++preservedInPlaceCount;
      preservedInPlaceSize += length;
    }

    AVER(p < q);
    p = q;
  }
  AVER(p == limit);
  ShieldCover(arena, seg);

adjustColour:
  SegSetNailed(seg, TraceSetDel(SegNailed(seg), trace));
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  if (SegNailed(seg) == TraceSetEMPTY && AMCSegHasNailBoard(seg)) {
    AMCSegDestroyNailBoard(seg, pool);
  }

  AVER(bytesReclaimed <= SegSize(seg));
  trace->reclaimSize += bytesReclaimed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;
}


/* AMCReclaim -- recycle a segment if it is still white
 *
 * See design.mps.poolamc.reclaim.
 */

static void AMCReclaim(Pool pool, Trace trace, Seg seg)
{
  AMC amc;
  AMCGen gen;
  Size size;

  AVERT_CRITICAL(Pool, pool);
  amc = PoolPoolAMC(pool);
  AVERT_CRITICAL(AMC, amc);
  AVERT_CRITICAL(Trace, trace);
  AVERT_CRITICAL(Seg, seg);

  gen = AMCSegGen(seg);
  AVERT_CRITICAL(AMCGen, gen);

  EVENT_PPP(AMCReclaim, gen, trace, seg);

  /* This switching needs to be more complex for multiple traces. */
  AVER_CRITICAL(TraceSetIsSingle(PoolArena(pool)->busyTraces));
  if (amc->rampMode == collectingRamp) {
    if (amc->rampCount > 0)
      /* Entered ramp mode before previous one was cleaned up */
      amc->rampMode = beginRamp;
    else
      amc->rampMode = outsideRamp;
  }

  if (SegNailed(seg) != TraceSetEMPTY) {
    AMCReclaimNailed(pool, trace, seg);
    return;
  }

  --gen->segs;
  size = SegSize(seg);
  gen->pgen.totalSize -= size;

  trace->reclaimSize += size;

  SegFree(seg);
}


/* AMCWalk -- Apply function to (black) objects in segment */

static void AMCWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                    void *p, unsigned long s)
{
    Addr object;
    Addr nextObject;
    Addr limit;
    AMC amc;
    Format format;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures so can't be checked */

  /* Avoid applying the function to grey or white objects. */
  /* White objects might not be alive, and grey objects */
  /* may have pointers to old-space. */

  /* NB, segments containing a mix of colours (i.e., nailed segs) */
  /* are not handled properly:  No objects are walked @@@@ */
  if (SegWhite(seg) == TraceSetEMPTY && SegGrey(seg) == TraceSetEMPTY
      && SegNailed(seg) == TraceSetEMPTY) {
    amc = PoolPoolAMC(pool);
    AVERT(AMC, amc);
    format = pool->format;

    /* If the segment is buffered, only walk as far as the end */
    /* of the initialized objects.  cf. AMCScan */
    if (SegBuffer(seg) != NULL)
      limit = BufferScanLimit(SegBuffer(seg));
    else
      limit = SegLimit(seg);
    limit = AddrAdd(limit, format->headerSize);

    object = AddrAdd(SegBase(seg), format->headerSize);
    while(object < limit) {
      /* Check not a broken heart. */
      AVER((*format->isMoved)(object) == NULL);
      (*f)(object, pool->format, pool, p, s);
      nextObject = (*pool->format->skip)(object);
      AVER(nextObject > object);
      object = nextObject;
    }
    AVER(object == limit);
  }
}


/* AMCWalkAll -- Apply a function to all (black) objects in a pool */

static void AMCWalkAll(Pool pool, FormattedObjectsStepMethod f,
                       void *p, unsigned long s)
{
  Arena arena;
  Ring ring, next, node;

  AVERT(Pool, pool);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */
  AVER(IsSubclassPoly(pool->class, EnsureAMCPoolClass()));

  arena = PoolArena(pool);

  ring = PoolSegRing(pool);
  node = RingNext(ring);
  RING_FOR(node, ring, next) {
    Seg seg = SegOfPoolRing(node);

    ShieldExpose(arena, seg);
    AMCWalk(pool, seg, f, p, s);
    ShieldCover(arena, seg);
  }
}


/* AMCDescribe -- describe the contents of the AMC pool
 *
 * See design.mps.poolamc.describe.
 */

static Res AMCDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  AMC amc;
  Ring node, nextNode;
  char *rampmode;

  if (!CHECKT(Pool, pool)) return ResFAIL;
  amc = PoolPoolAMC(pool);
  if (!CHECKT(AMC, amc)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               (amc->rankSet == RankSetEMPTY) ? "AMCZ" : "AMC",
               " $P {\n", (WriteFP)amc, "  pool $P ($U)\n",
               (WriteFP)AMCPool(amc), (WriteFU)AMCPool(amc)->serial,
               NULL);
  if (res != ResOK) return res;

  switch(amc->rampMode) {
  case outsideRamp: rampmode = "outside ramp"; break;
  case beginRamp: rampmode = "begin ramp"; break;
  case ramping: rampmode = "ramping"; break;
  case finishRamp: rampmode = "finish ramp"; break;
  case collectingRamp: rampmode = "collecting ramp"; break;
  default: rampmode = "unknown ramp mode"; break;
  }
  res = WriteF(stream,
               "  ", rampmode, " ($U)\n", (WriteFU)amc->rampCount,
               NULL);
  if (res != ResOK) return res;

  RING_FOR(node, &amc->genRing, nextNode) {
    AMCGen gen = RING_ELT(AMCGen, amcRing, node);
    res = AMCGenDescribe(gen, stream);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, "} AMC $P\n", (WriteFP)amc, NULL);
  if (res != ResOK) return res;

  return ResOK;
}


/* AMCPoolClass -- the class definition */

DEFINE_POOL_CLASS(AMCPoolClass, this)
{
  INHERIT_CLASS(this, AbstractCollectPoolClass);
  PoolClassMixInFormat(this);
  this->name = "AMC";
  this->size = sizeof(AMCStruct);
  this->offset = offsetof(AMCStruct, poolStruct);
  this->attr |= AttrMOVINGGC;
  this->init = AMCInit;
  this->finish = AMCFinish;
  this->bufferFill = AMCBufferFill;
  this->bufferEmpty = AMCBufferEmpty;
  this->whiten = AMCWhiten;
  this->scan = AMCScan;
  this->fix = AMCFix;
  this->fixEmergency = AMCFixEmergency;
  this->reclaim = AMCReclaim;
  this->rampBegin = AMCRampBegin;
  this->rampEnd = AMCRampEnd;
  this->walk = AMCWalk;
  this->bufferClass = EnsureAMCBufClass;
  this->describe = AMCDescribe;
}


/* AMCZPoolClass -- the class definition */

DEFINE_POOL_CLASS(AMCZPoolClass, this)
{
  INHERIT_CLASS(this, AMCPoolClass);
  this->name = "AMCZ";
  this->attr &= ~(AttrSCAN | AttrINCR_RB);
  this->init = AMCZInit;
  this->grey = PoolNoGrey;
  this->scan = PoolNoScan;
}
  

/* mps_class_amc -- return the pool class descriptor to the client */

mps_class_t mps_class_amc(void)
{
  return (mps_class_t)(EnsureAMCPoolClass());
}

/* mps_class_amcz -- return the pool class descriptor to the client */

mps_class_t mps_class_amcz(void)
{
  return (mps_class_t)(EnsureAMCZPoolClass());
}


/* mps_amc_apply -- apply function to all objects in pool
 *
 * The iterator that is passed by the client is stored in a closure
 * structure which is passed to a local iterator in order to ensure that
 * any type conversion necessary between Addr and mps_addr_t happen.
 * They are almost certainly the same on all platforms, but this is the
 * correct way to do it.
*/

typedef struct mps_amc_apply_closure_s {
  void (*f)(mps_addr_t object, void *p, size_t s);
  void *p;
  size_t s;
} mps_amc_apply_closure_s;

static void mps_amc_apply_iter(Addr addr, Format format, Pool pool,
                               void *p, unsigned long s)
{
  mps_amc_apply_closure_s *closure = p;
  /* Can't check addr */
  AVERT(Format, format);
  AVERT(Pool, pool);
  /* We could check that s is the sizeof *p, but it would be slow */
  UNUSED(format);
  UNUSED(pool);
  UNUSED(s);
  (*closure->f)(addr, closure->p, closure->s);
}

void mps_amc_apply(mps_pool_t mps_pool,
                   void (*f)(mps_addr_t object, void *p, size_t s),
                   void *p, size_t s)
{
  Pool pool = (Pool)mps_pool;
  mps_amc_apply_closure_s closure_s;
  Arena arena;

  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);
  AVERT(Pool, pool);

  closure_s.f = f; closure_s.p = p; closure_s.s = s;
  AMCWalkAll(pool, mps_amc_apply_iter, &closure_s, sizeof(closure_s));

  ArenaLeave(arena);
}


/* AMCCheck -- check consistency of the AMC pool
 *
 * See design.mps.poolamc.check.
 */

static Bool AMCCheck(AMC amc)
{
  CHECKS(AMC, amc);
  CHECKD(Pool, &amc->poolStruct);
  CHECKL(IsSubclassPoly(amc->poolStruct.class, EnsureAMCPoolClass()));
  CHECKL(RankSetCheck(amc->rankSet));
  CHECKL(RingCheck(&amc->genRing));
  CHECKL(BoolCheck(amc->gensBooted));
  if (amc->gensBooted) {
    CHECKD(AMCGen, amc->nursery);
    CHECKL(amc->gen != NULL);
    CHECKD(AMCGen, amc->rampGen);
    CHECKD(AMCGen, amc->afterRampGen);
  }
  /* nothing to check for rampCount */
  CHECKL(amc->rampMode >= outsideRamp && amc->rampMode <= collectingRamp);
  CHECKL(BoolCheck(amc->collectAll));
  CHECKL(BoolCheck(amc->collectAllNext));

  return TRUE;
}

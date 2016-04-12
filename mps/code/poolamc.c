/* poolamc.c: AUTOMATIC MOSTLY-COPYING MEMORY POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .sources: <design/poolamc/>.
 */

#include "mpscamc.h"
#include "locus.h"
#include "bt.h"
#include "mpm.h"
#include "nailboard.h"

SRCID(poolamc, "$Id$");

typedef struct AMCStruct *AMC;
typedef struct amcGenStruct *amcGen;

/* Function returning TRUE if block in nailboarded segment is pinned. */
typedef Bool (*amcPinnedFunction)(AMC amc, Nailboard board, Addr base, Addr limit);

/* forward declarations */

static Bool amcSegHasNailboard(Seg seg);
static Nailboard amcSegNailboard(Seg seg);
static Bool AMCCheck(AMC amc);
static Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);

/* local class declations */

typedef AMC AMCZPool;
#define AMCZPoolCheck AMCCheck
DECLARE_CLASS(Pool, AMCZPool);

typedef AMC AMCPool;
DECLARE_CLASS(Pool, AMCPool);

DECLARE_CLASS(Buffer, amcBuf);
DECLARE_CLASS(Seg, amcSeg);


/* amcGenStruct -- pool AMC generation descriptor */

#define amcGenSig       ((Sig)0x519A3C9E)  /* SIGnature AMC GEn */

typedef struct amcGenStruct {
  PoolGenStruct pgen;
  RingStruct amcRing;           /* link in list of gens in pool */
  Buffer forward;               /* forwarding buffer */
  Sig sig;                      /* <code/misc.h#sig> */
} amcGenStruct;

#define amcGenAMC(amcgen) MustBeA(AMCZPool, (amcgen)->pgen.pool)
#define amcGenPool(amcgen) ((amcgen)->pgen.pool)

#define amcGenNr(amcgen) ((amcgen)->pgen.nr)


#define RAMP_RELATION(X)                        \
  X(RampOUTSIDE,        "outside ramp")         \
  X(RampBEGIN,          "begin ramp")           \
  X(RampRAMPING,        "ramping")              \
  X(RampFINISH,         "finish ramp")          \
  X(RampCOLLECTING,     "collecting ramp")

#define RAMP_ENUM(e, s) e,
enum {
    RAMP_RELATION(RAMP_ENUM)
    RampLIMIT
};
#undef RAMP_ENUM


/* amcSegStruct -- AMC-specific fields appended to GCSegStruct
 *
 * .seg.old: The "old" flag is FALSE if the segment has never been
 * collected, and so its size is accounted against the pool
 * generation's newSize; it is TRUE if the segment has been collected
 * at least once, and so its size is accounted against the pool
 * generation's oldSize.
 *
 * .seg.deferred: The "deferred" flag is TRUE if its size accounting
 * in the pool generation has been deferred. This is set if the
 * segment was created in ramping mode (and so we don't want it to
 * contribute to the pool generation's newSize and so provoke a
 * collection via TracePoll), and by hash array allocations (where we
 * don't want the allocation to provoke a collection that makes the
 * location dependency stale immediately).
 */

typedef struct amcSegStruct *amcSeg;

#define amcSegSig      ((Sig)0x519A3C59) /* SIGnature AMC SeG */

typedef struct amcSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  amcGen gen;               /* generation this segment belongs to */
  Nailboard board;          /* nailboard for this segment or NULL if none */
  BOOLFIELD(old);           /* .seg.old */
  BOOLFIELD(deferred);      /* .seg.deferred */
  Sig sig;                  /* <code/misc.h#sig> */
} amcSegStruct;

#define Seg2amcSeg(seg)             ((amcSeg)(seg))
#define amcSeg2Seg(amcseg)          ((Seg)(amcseg))


ATTRIBUTE_UNUSED
static Bool amcSegCheck(amcSeg amcseg)
{
  CHECKS(amcSeg, amcseg);
  CHECKD(GCSeg, &amcseg->gcSegStruct);
  CHECKU(amcGen, amcseg->gen);
  if (amcseg->board) {
    CHECKD(Nailboard, amcseg->board);
    CHECKL(SegNailed(amcSeg2Seg(amcseg)) != TraceSetEMPTY);
  }
  /* CHECKL(BoolCheck(amcseg->old)); <design/type/#bool.bitfield.check> */
  /* CHECKL(BoolCheck(amcseg->deferred)); <design/type/#bool.bitfield.check> */
  return TRUE;
}


/* AMCSegInit -- initialise an AMC segment */

ARG_DEFINE_KEY(amc_seg_gen, Pointer);
#define amcKeySegGen (&_mps_key_amc_seg_gen)

static Res AMCSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  amcGen amcgen;
  amcSeg amcseg;
  Res res;
  ArgStruct arg;
  
  ArgRequire(&arg, args, amcKeySegGen);
  amcgen = arg.val.p;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, amcSeg, init)(seg, pool, base, size, args);
  if(res != ResOK)
    return res;
  amcseg = CouldBeA(amcSeg, seg);

  amcseg->gen = amcgen;
  amcseg->board = NULL;
  amcseg->old = FALSE;
  amcseg->deferred = FALSE;

  SetClassOfPoly(seg, CLASS(amcSeg));
  amcseg->sig = amcSegSig;
  AVERC(amcSeg, amcseg);

  return ResOK;
}


/* AMCSegSketch -- summarise the segment state for a human reader
 *
 * Write a short human-readable text representation of the segment 
 * state into storage indicated by pbSketch+cbSketch.
 *
 * A typical sketch is "bGW_", meaning the seg has a nailboard, has 
 * some Grey and some White objects, and has no buffer attached.
 */

static void AMCSegSketch(Seg seg, char *pbSketch, size_t cbSketch)
{
  amcSeg amcseg;
  Buffer buffer;

  AVER(pbSketch);
  AVER(cbSketch >= 5);
  AVERT(Seg, seg);
  amcseg = Seg2amcSeg(seg);
  AVERT(amcSeg, amcseg);

  if(SegNailed(seg) == TraceSetEMPTY) {
    pbSketch[0] = 'm';  /* mobile */
  } else if (amcSegHasNailboard(seg)) {
    pbSketch[0] = 'b';  /* boarded */
  } else {
    pbSketch[0] = 's';  /* stuck */
  }

  if(SegGrey(seg) == TraceSetEMPTY) {
    pbSketch[1] = '_';
  } else {
    pbSketch[1] = 'G';  /* Grey */
  }

  if(SegWhite(seg) == TraceSetEMPTY) {
    pbSketch[2] = '_';
  } else {
    pbSketch[2] = 'W';  /* White */
  }

  buffer = SegBuffer(seg);
  if(buffer == NULL) {
    pbSketch[3] = '_';
  } else {
    Bool mut = BufferIsMutator(buffer);
    Bool flipped = ((buffer->mode & BufferModeFLIPPED) != 0);
    Bool trapped = BufferIsTrapped(buffer);
    Bool limitzeroed = (buffer->ap_s.limit == 0);

    pbSketch[3] = 'X';  /* I don't know what's going on! */

    if((flipped == trapped) && (trapped == limitzeroed)) {
      if(mut) {
        if(flipped) {
          pbSketch[3] = 's';  /* stalo */
        } else {
          pbSketch[3] = 'n';  /* neo */
        }
      } else {
        if(!flipped) {
          pbSketch[3] = 'f';  /* forwarding */
        }
      }
    } else {
      /* I don't know what's going on! */
    }
  }
  
  pbSketch[4] = '\0';
  AVER(4 < cbSketch);
}


/* AMCSegDescribe -- describe the contents of a segment
 *
 * See <design/poolamc/#seg-describe>.
 */
static Res AMCSegDescribe(Seg seg, mps_lib_FILE *stream, Count depth)
{
  Res res;
  Pool pool;
  amcSeg amcseg;
  Addr i, p, base, limit, init;
  Align step;
  Size row;
  char abzSketch[5];

  if(!TESTT(Seg, seg))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;
  amcseg = Seg2amcSeg(seg);
  if(!TESTT(amcSeg, amcseg))
    return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  res = NextMethod(Seg, amcSeg, describe)(seg, stream, depth);
  if(res != ResOK)
    return res;

  pool = SegPool(seg);
  step = PoolAlignment(pool);
  row = step * 64;

  base = SegBase(seg);
  p = AddrAdd(base, pool->format->headerSize);
  limit = SegLimit(seg);

  res = WriteF(stream, depth,
               "AMC seg $P [$A,$A){\n",
               (WriteFP)seg, (WriteFA)base, (WriteFA)limit,
               NULL);
  if(res != ResOK)
    return res;

  if(amcSegHasNailboard(seg)) {
    res = WriteF(stream, depth + 2, "Boarded\n", NULL);
  } else if(SegNailed(seg) == TraceSetEMPTY) {
    res = WriteF(stream, depth + 2, "Mobile\n", NULL);
  } else {
    res = WriteF(stream, depth + 2, "Stuck\n", NULL);
  }
  if(res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "Map:  *===:object  @+++:nails  bbbb:buffer\n", NULL);
  if(res != ResOK)
    return res;

  if(SegBuffer(seg) != NULL)
    init = BufferGetInit(SegBuffer(seg));
  else
    init = limit;
  
  for(i = base; i < limit; i = AddrAdd(i, row)) {
    Addr j;
    char c;

    res = WriteF(stream, depth + 2, "$A  ", (WriteFA)i, NULL);
    if(res != ResOK)
      return res;

    /* @@@@ This misses a header-sized pad at the end. */
    for(j = i; j < AddrAdd(i, row); j = AddrAdd(j, step)) {
      if(j >= limit)
        c = ' ';  /* if seg is not a whole number of print rows */
      else if(j >= init)
        c = 'b';
      else {
        Bool nailed = amcSegHasNailboard(seg)
          && NailboardGet(amcSegNailboard(seg), j);
        if(j == p) {
          c = (nailed ? '@' : '*');
          p = (pool->format->skip)(p);
        } else {
          c = (nailed ? '+' : '=');
        }
      }
      res = WriteF(stream, 0, "$C", (WriteFC)c, NULL);
      if(res != ResOK)
        return res;
    }

    res = WriteF(stream, 0, "\n", NULL);
    if(res != ResOK)
      return res;
  }

  AMCSegSketch(seg, abzSketch, NELEMS(abzSketch));
  res = WriteF(stream, depth + 2, "Sketch: $S\n", (WriteFS)abzSketch, NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream, depth, "} AMC Seg $P\n", (WriteFP)seg, NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* amcSegClass -- Class definition for AMC segments */

DEFINE_CLASS(Seg, amcSeg, class)
{
  INHERIT_CLASS(class, amcSeg, GCSeg);
  SegClassMixInNoSplitMerge(class);  /* no support for this (yet) */
  class->size = sizeof(amcSegStruct);
  class->init = AMCSegInit;
  class->describe = AMCSegDescribe;
}



/* amcSegHasNailboard -- test whether the segment has a nailboard
 *
 * See <design/poolamc/#fix.nail.distinguish>.
 */
static Bool amcSegHasNailboard(Seg seg)
{
  amcSeg amcseg = Seg2amcSeg(seg);
  return amcseg->board != NULL;
}


/* amcSegNailboard -- get the nailboard for this segment */

static Nailboard amcSegNailboard(Seg seg)
{
  amcSeg amcseg = Seg2amcSeg(seg);
  AVER(amcSegHasNailboard(seg));
  return amcseg->board;
}


/* amcSegGen -- get the generation structure for this segment */

static amcGen amcSegGen(Seg seg)
{
  amcSeg amcseg = Seg2amcSeg(seg);
  return amcseg->gen;
}


/* AMCStruct -- pool AMC descriptor
 *
 * See <design/poolamc/#struct>.
 */

#define AMCSig          ((Sig)0x519A3C99) /* SIGnature AMC */

typedef struct AMCStruct { /* <design/poolamc/#struct> */
  PoolStruct poolStruct;   /* generic pool structure */
  RankSet rankSet;         /* rankSet for entire pool */
  RingStruct genRing;      /* ring of generations */
  Bool gensBooted;         /* used during boot (init) */
  size_t gens;             /* number of generations */
  amcGen *gen;             /* (pointer to) array of generations */
  amcGen nursery;          /* the default mutator generation */
  amcGen rampGen;          /* the ramp generation */
  amcGen afterRampGen;     /* the generation after rampGen */
  unsigned rampCount;      /* <design/poolamc/#ramp.count> */
  int rampMode;            /* <design/poolamc/#ramp.mode> */
  amcPinnedFunction pinned; /* function determining if block is pinned */
  Size extendBy;           /* segment size to extend pool by */
  Size largeSize;          /* min size of "large" segments */
  Sig sig;                 /* <design/pool/#outer-structure.sig> */
} AMCStruct;


/* amcGenCheck -- check consistency of a generation structure */

ATTRIBUTE_UNUSED
static Bool amcGenCheck(amcGen gen)
{
  AMC amc;

  CHECKS(amcGen, gen);
  CHECKD(PoolGen, &gen->pgen);
  amc = amcGenAMC(gen);
  CHECKU(AMC, amc);
  CHECKD(Buffer, gen->forward);
  CHECKD_NOSIG(Ring, &gen->amcRing);

  return TRUE;
}


/* amcBufStruct -- AMC Buffer subclass
 *
 * This subclass of SegBuf records a link to a generation.
 */

#define amcBufSig ((Sig)0x519A3CBF) /* SIGnature AMC BuFfer  */

typedef struct amcBufStruct *amcBuf;

typedef struct amcBufStruct {
  SegBufStruct segbufStruct;    /* superclass fields must come first */
  amcGen gen;                   /* The AMC generation */
  Bool forHashArrays;           /* allocates hash table arrays, see AMCBufferFill */
  Sig sig;                      /* <design/sig/> */
} amcBufStruct;


/* amcBufCheck -- check consistency of an amcBuf */

ATTRIBUTE_UNUSED
static Bool amcBufCheck(amcBuf amcbuf)
{
  CHECKS(amcBuf, amcbuf);
  CHECKD(SegBuf, &amcbuf->segbufStruct);
  if(amcbuf->gen != NULL)
    CHECKD(amcGen, amcbuf->gen);
  CHECKL(BoolCheck(amcbuf->forHashArrays));
  /* hash array buffers only created by mutator */
  CHECKL(BufferIsMutator(MustBeA(Buffer, amcbuf)) || !amcbuf->forHashArrays);
  return TRUE;
}


/* amcBufGen -- Return the AMC generation of an amcBuf */

static amcGen amcBufGen(Buffer buffer)
{
  return MustBeA(amcBuf, buffer)->gen;
}


/* amcBufSetGen -- Set the AMC generation of an amcBuf */

static void amcBufSetGen(Buffer buffer, amcGen gen)
{
  amcBuf amcbuf = MustBeA(amcBuf, buffer);
  if (gen != NULL)
    AVERT(amcGen, gen);
  amcbuf->gen = gen;
}


ARG_DEFINE_KEY(ap_hash_arrays, Bool);

#define amcKeyAPHashArrays (&_mps_key_ap_hash_arrays)

/* AMCBufInit -- Initialize an amcBuf */

static Res AMCBufInit(Buffer buffer, Pool pool, Bool isMutator, ArgList args)
{
  AMC amc = MustBeA(AMCZPool, pool);
  amcBuf amcbuf;
  Res res;
  Bool forHashArrays = FALSE;
  ArgStruct arg;

  if (ArgPick(&arg, args, amcKeyAPHashArrays))
    forHashArrays = arg.val.b;

  /* call next method */
  res = NextMethod(Buffer, amcBuf, init)(buffer, pool, isMutator, args);
  if(res != ResOK)
    return res;
  amcbuf = CouldBeA(amcBuf, buffer);

  if (BufferIsMutator(buffer)) {
    /* Set up the buffer to be allocating in the nursery. */
    amcbuf->gen = amc->nursery;
  } else {
    /* No gen yet -- see <design/poolamc/#gen.forward>. */
    amcbuf->gen = NULL;
  }
  amcbuf->forHashArrays = forHashArrays;

  SetClassOfPoly(buffer, CLASS(amcBuf));
  amcbuf->sig = amcBufSig;
  AVERC(amcBuf, amcbuf);

  BufferSetRankSet(buffer, amc->rankSet);

  return ResOK;
}


/* AMCBufFinish -- Finish an amcBuf */

static void AMCBufFinish(Buffer buffer)
{
  amcBuf amcbuf = MustBeA(amcBuf, buffer);
  amcbuf->sig = SigInvalid;
  NextMethod(Buffer, amcBuf, finish)(buffer);
}


/* amcBufClass -- The class definition */

DEFINE_CLASS(Buffer, amcBuf, class)
{
  INHERIT_CLASS(class, amcBuf, SegBuf);
  class->size = sizeof(amcBufStruct);
  class->init = AMCBufInit;
  class->finish = AMCBufFinish;
}


/* amcGenCreate -- create a generation */

static Res amcGenCreate(amcGen *genReturn, AMC amc, GenDesc gen)
{
  Pool pool = MustBeA(AbstractPool, amc);
  Arena arena;
  Buffer buffer;
  amcGen amcgen;
  Res res;
  void *p;

  arena = pool->arena;

  res = ControlAlloc(&p, arena, sizeof(amcGenStruct));
  if(res != ResOK)
    goto failControlAlloc;
  amcgen = (amcGen)p;

  res = BufferCreate(&buffer, CLASS(amcBuf), pool, FALSE, argsNone);
  if(res != ResOK)
    goto failBufferCreate;

  res = PoolGenInit(&amcgen->pgen, gen, pool);
  if(res != ResOK)
    goto failGenInit;
  RingInit(&amcgen->amcRing);
  amcgen->forward = buffer;
  amcgen->sig = amcGenSig;

  AVERT(amcGen, amcgen);

  RingAppend(&amc->genRing, &amcgen->amcRing);
  EVENT2(AMCGenCreate, amc, amcgen);
  *genReturn = amcgen;
  return ResOK;

failGenInit:
  BufferDestroy(buffer);
failBufferCreate:
  ControlFree(arena, p, sizeof(amcGenStruct));
failControlAlloc:
  return res;
}


/* amcGenDestroy -- destroy a generation */

static void amcGenDestroy(amcGen gen)
{
  Arena arena;

  AVERT(amcGen, gen);

  EVENT1(AMCGenDestroy, gen);
  arena = PoolArena(amcGenPool(gen));
  gen->sig = SigInvalid;
  RingRemove(&gen->amcRing);
  RingFinish(&gen->amcRing);
  PoolGenFinish(&gen->pgen);
  BufferDestroy(gen->forward);
  ControlFree(arena, gen, sizeof(amcGenStruct));
}


/* amcGenDescribe -- describe an AMC generation */

static Res amcGenDescribe(amcGen gen, mps_lib_FILE *stream, Count depth)
{
  Res res;

  if(!TESTT(amcGen, gen))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, depth,
               "amcGen $P {\n", (WriteFP)gen,
               "  buffer $P\n", (WriteFP)gen->forward, NULL);
  if (res != ResOK)
    return res;

  res = PoolGenDescribe(&gen->pgen, stream, depth + 2);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth, "} amcGen $P\n", (WriteFP)gen, NULL);
  return res;
}


/* amcSegCreateNailboard -- create nailboard for segment */

static Res amcSegCreateNailboard(Seg seg, Pool pool)
{
  amcSeg amcseg;
  Nailboard board;
  Arena arena;
  Res res;

  amcseg = Seg2amcSeg(seg);
  AVER(!amcSegHasNailboard(seg));
  arena = PoolArena(pool);

  res = NailboardCreate(&board, arena, pool->alignment, 
                        SegBase(seg), SegLimit(seg));
  if (res != ResOK)
    return res;
  amcseg->board = board;
  return ResOK;
}


/* amcPinnedInterior -- block is pinned by any nail */

static Bool amcPinnedInterior(AMC amc, Nailboard board, Addr base, Addr limit)
{
  Size headerSize = MustBeA(AbstractPool, amc)->format->headerSize;
  return !NailboardIsResRange(board, AddrSub(base, headerSize),
                              AddrSub(limit, headerSize));
}


/* amcPinnedBase -- block is pinned only if base is nailed */

static Bool amcPinnedBase(AMC amc, Nailboard board, Addr base, Addr limit)
{
  UNUSED(amc);
  UNUSED(limit);
  return NailboardGet(board, base);
}


/* amcVarargs -- decode obsolete varargs */

static void AMCVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_CHAIN;
  args[1].val.chain = va_arg(varargs, Chain);
  args[2].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* amcInitComm -- initialize AMC/Z pool
 *
 * See <design/poolamc/#init>.
 * Shared by AMCInit and AMCZinit.
 */
static Res amcInitComm(Pool pool, Arena arena, PoolClass class,
                       RankSet rankSet, ArgList args)
{
  AMC amc;
  Res res;
  Index i;
  size_t genArraySize;
  size_t genCount;
  Bool interior = AMC_INTERIOR_DEFAULT;
  Chain chain;
  Size extendBy = AMC_EXTEND_BY_DEFAULT;
  Size largeSize = AMC_LARGE_SIZE_DEFAULT;
  ArgStruct arg;
  Format format;
  
  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  AVERT(PoolClass, class);
  AVER(IsSubclass(class, AMCZPool));
  
  ArgRequire(&arg, args, MPS_KEY_FORMAT);
  format = arg.val.format;
  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else
    chain = ArenaGlobals(arena)->defaultChain;
  if (ArgPick(&arg, args, MPS_KEY_INTERIOR))
    interior = arg.val.b;
  if (ArgPick(&arg, args, MPS_KEY_EXTEND_BY))
    extendBy = arg.val.size;
  if (ArgPick(&arg, args, MPS_KEY_LARGE_SIZE))
    largeSize = arg.val.size;
  
  AVERT(Format, format);
  AVER(FormatArena(format) == arena);
  AVERT(Chain, chain);
  AVER(chain->arena == arena);
  AVER(extendBy > 0);
  AVER(largeSize > 0);
  /* TODO: it would be nice to be able to manage large objects that
   * are smaller than the extendBy, but currently this results in
   * unacceptable fragmentation due to the padding objects. This
   * assertion catches this bad case. */
  AVER(largeSize >= extendBy);

  res = PoolAbsInit(pool, arena, class, args);
  if (res != ResOK)
    return res;
  amc = CouldBeA(AMCZPool, pool);

  pool->format = format;
  pool->alignment = pool->format->alignment;
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
  amc->rampMode = RampOUTSIDE;

  if (interior) {
    amc->pinned = amcPinnedInterior;
  } else {
    amc->pinned = amcPinnedBase;
  }
  /* .extend-by.aligned: extendBy is aligned to the arena alignment. */
  amc->extendBy = SizeArenaGrains(extendBy, arena);
  amc->largeSize = largeSize;

  SetClassOfPoly(pool, class);
  amc->sig = AMCSig;
  AVERC(AMCZPool, amc);

  /* Init generations. */
  genCount = ChainGens(chain);
  {
    void *p;

    /* One gen for each one in the chain plus dynamic gen. */
    genArraySize = sizeof(amcGen) * (genCount + 1);
    res = ControlAlloc(&p, arena, genArraySize);
    if(res != ResOK)
      goto failGensAlloc;
    amc->gen = p;
    for (i = 0; i <= genCount; ++i) {
      res = amcGenCreate(&amc->gen[i], amc, ChainGen(chain, i));
      if (res != ResOK)
        goto failGenAlloc;
    }
    /* Set up forwarding buffers. */
    for(i = 0; i < genCount; ++i) {
      amcBufSetGen(amc->gen[i]->forward, amc->gen[i+1]);
    }
    /* Dynamic gen forwards to itself. */
    amcBufSetGen(amc->gen[genCount]->forward, amc->gen[genCount]);
  }
  amc->nursery = amc->gen[0];
  amc->rampGen = amc->gen[genCount-1]; /* last ephemeral gen */
  amc->afterRampGen = amc->gen[genCount];
  amc->gensBooted = TRUE;

  AVERT(AMC, amc);
  EVENT2(AMCInit, pool, amc);
  if(rankSet == RankSetEMPTY)
    EVENT2(PoolInitAMCZ, pool, pool->format);
  else
    EVENT2(PoolInitAMC, pool, pool->format);
  return ResOK;

failGenAlloc:
  while(i > 0) {
    --i;
    amcGenDestroy(amc->gen[i]);
  }
  ControlFree(arena, amc->gen, genArraySize);
failGensAlloc:
  PoolAbsFinish(pool);
  return res;
}

/* TODO: AMCInit should call AMCZInit (its superclass) then
   specialize, but amcInitComm creates forwarding buffers that copy
   the rank set from the pool, making this awkward. */

static Res AMCInit(Pool pool, Arena arena, PoolClass class, ArgList args)
{
  UNUSED(class); /* used for debug pools only */
  return amcInitComm(pool, arena, CLASS(AMCPool), RankSetSingle(RankEXACT), args);
}

static Res AMCZInit(Pool pool, Arena arena, PoolClass class, ArgList args)
{
  UNUSED(class); /* used for debug pools only */
  return amcInitComm(pool, arena, CLASS(AMCZPool), RankSetEMPTY, args);
}


/* AMCFinish -- finish AMC pool
 *
 * See <design/poolamc/#finish>.
 */
static void AMCFinish(Pool pool)
{
  AMC amc = MustBeA(AMCZPool, pool);
  Ring ring;
  Ring node, nextNode;

  EVENT1(AMCFinish, amc);

  /* @@@@ Make sure that segments aren't buffered by forwarding */
  /* buffers.  This is a hack which allows the pool to be destroyed */
  /* while it is collecting.  Note that there aren't any mutator */
  /* buffers by this time. */
  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    BufferDetach(gen->forward, pool);
  }

  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    amcGen gen = amcSegGen(seg);
    amcSeg amcseg = Seg2amcSeg(seg);
    AVERT(amcSeg, amcseg);
    PoolGenFree(&gen->pgen, seg,
                0,
                amcseg->old ? SegSize(seg) : 0,
                amcseg->old ? 0 : SegSize(seg),
                amcseg->deferred);
  }

  /* Disassociate forwarding buffers from gens before they are */
  /* destroyed. */
  ring = &amc->genRing;
  RING_FOR(node, ring, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    amcBufSetGen(gen->forward, NULL);
  }
  RING_FOR(node, ring, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    amcGenDestroy(gen);
  }

  amc->sig = SigInvalid;
  PoolAbsFinish(pool);
}


/* AMCBufferFill -- refill an allocation buffer
 *
 * See <design/poolamc/#fill>.
 */
static Res AMCBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Seg seg;
  AMC amc = MustBeA(AMCZPool, pool);
  Res res;
  Addr base, limit;
  Arena arena;
  Size grainsSize;
  amcGen gen;
  PoolGen pgen;
  amcBuf amcbuf = MustBeA(amcBuf, buffer);

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  arena = PoolArena(pool);
  gen = amcBufGen(buffer);
  AVERT(amcGen, gen);
  pgen = &gen->pgen;

  /* Create and attach segment.  The location of this segment is */
  /* expressed via the pool generation. We rely on the arena to */
  /* organize locations appropriately.  */
  if (size < amc->extendBy) {
    grainsSize = amc->extendBy; /* .extend-by.aligned */
  } else {
    grainsSize = SizeArenaGrains(size, arena);
  }
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD_FIELD(args, amcKeySegGen, p, gen);
    res = PoolGenAlloc(&seg, pgen, CLASS(amcSeg), grainsSize, args);
  } MPS_ARGS_END(args);
  if(res != ResOK)
    return res;
  AVER(grainsSize == SegSize(seg));

  /* <design/seg/#field.rankSet.start> */
  if(BufferRankSet(buffer) == RankSetEMPTY)
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  else
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);

  /* If ramping, or if the buffer is intended for allocating hash
   * table arrays, defer the size accounting. */
  if ((amc->rampMode == RampRAMPING
       && buffer == amc->rampGen->forward
       && gen == amc->rampGen)
      || amcbuf->forHashArrays) 
  {
    Seg2amcSeg(seg)->deferred = TRUE;
  }

  base = SegBase(seg);
  if (size < amc->largeSize) {
    /* Small or Medium segment: give the buffer the entire seg. */
    limit = AddrAdd(base, grainsSize);
    AVER(limit == SegLimit(seg));
  } else {
    /* Large segment: ONLY give the buffer the size requested, and */
    /* pad the remainder of the segment: see job001811. */
    Size padSize;

    limit = AddrAdd(base, size);
    AVER(limit <= SegLimit(seg));
    
    padSize = grainsSize - size;
    AVER(SizeIsAligned(padSize, PoolAlignment(pool)));
    AVER(AddrAdd(limit, padSize) == SegLimit(seg));
    if(padSize > 0) {
      ShieldExpose(arena, seg);
      (*pool->format->pad)(limit, padSize);
      ShieldCover(arena, seg);
    }
  }

  PoolGenAccountForFill(pgen, SegSize(seg), Seg2amcSeg(seg)->deferred);
  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* amcBufferEmpty -- detach a buffer from a segment
 *
 * See <design/poolamc/#flush>.
 */
static void AMCBufferEmpty(Pool pool, Buffer buffer,
                           Addr init, Addr limit)
{
  AMC amc = MustBeA(AMCZPool, pool);
  Size size;
  Arena arena;
  Seg seg;

  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);

  arena = BufferArena(buffer);
  if(SegSize(seg) < amc->largeSize) {
    /* Small or Medium segment: buffer had the entire seg. */
    AVER(limit == SegLimit(seg));
  } else {
    /* Large segment: buffer had only the size requested; job001811. */
    AVER(limit <= SegLimit(seg));
  }

  /* <design/poolamc/#flush.pad> */
  size = AddrOffset(init, limit);
  if(size > 0) {
    ShieldExpose(arena, seg);
    (*pool->format->pad)(init, size);
    ShieldCover(arena, seg);
  }

  /* The unused part of the buffer is not reused by AMC, so we pass 0
   * for the unused argument. This call therefore has no effect on the
   * accounting, but we call it anyway for consistency. */
  PoolGenAccountForEmpty(&amcSegGen(seg)->pgen, 0, Seg2amcSeg(seg)->deferred);
}


/* AMCRampBegin -- note an entry into a ramp pattern */

static void AMCRampBegin(Pool pool, Buffer buf, Bool collectAll)
{
  AMC amc = MustBeA(AMCZPool, pool);

  AVERT(Buffer, buf);
  AVERT(Bool, collectAll);
  UNUSED(collectAll); /* obsolete */

  AVER(amc->rampCount < UINT_MAX);
  ++amc->rampCount;
  if(amc->rampCount == 1) {
    if(amc->rampMode != RampFINISH)
      amc->rampMode = RampBEGIN;
  }
}


/* AMCRampEnd -- note an exit from a ramp pattern */

static void AMCRampEnd(Pool pool, Buffer buf)
{
  AMC amc = MustBeA(AMCZPool, pool);

  AVERT(Buffer, buf);

  AVER(amc->rampCount > 0);
  --amc->rampCount;
  if(amc->rampCount == 0) {
    PoolGen pgen = &amc->rampGen->pgen;
    Ring node, nextNode;

    switch(amc->rampMode) {
      case RampRAMPING:
        /* We were ramping, so clean up. */
        amc->rampMode = RampFINISH;
        break;
      case RampBEGIN:
        /* short-circuit for short ramps */
        amc->rampMode = RampOUTSIDE;
        break;
      case RampCOLLECTING:
        /* we have finished a circuit of the state machine */
        amc->rampMode = RampOUTSIDE;
        break;
      case RampFINISH:
        /* stay in FINISH because we need to pass through COLLECTING */
        break;
      default:
        /* can't get here if already OUTSIDE */
        NOTREACHED;
    }

    /* Now all the segments in the ramp generation contribute to the
     * pool generation's sizes. */
    RING_FOR(node, PoolSegRing(pool), nextNode) {
      Seg seg = SegOfPoolRing(node);
      amcSeg amcseg = Seg2amcSeg(seg);
      if(amcSegGen(seg) == amc->rampGen
         && amcseg->deferred
         && SegWhite(seg) == TraceSetEMPTY)
      {
        PoolGenUndefer(pgen,
                       amcseg->old ? SegSize(seg) : 0,
                       amcseg->old ? 0 : SegSize(seg));
        amcseg->deferred = FALSE;
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
  Size condemned = 0;
  amcGen gen;
  AMC amc = MustBeA(AMCZPool, pool);
  Buffer buffer;
  amcSeg amcseg = MustBeA(amcSeg, seg);
  Res res;

  AVERT(Trace, trace);

  buffer = SegBuffer(seg);
  if(buffer != NULL) {
    AVERT(Buffer, buffer);

    if(!BufferIsMutator(buffer)) {      /* forwarding buffer */
      AVER(BufferIsReady(buffer));
      BufferDetach(buffer, pool);
    } else {                            /* mutator buffer */
      if(BufferScanLimit(buffer) == SegBase(seg)) {
        /* There's nothing but the buffer, don't condemn. */
        return ResOK;
      }
      /* [The following else-if section is just a comment added in */
      /*  1998-10-08.  It has never worked.  RHSK 2007-01-16] */
      /* else if (BufferScanLimit(buffer) == BufferLimit(buffer)) { */
        /* The buffer is full, so it won't be used by the mutator. */
        /* @@@@ We should detach it, but can't for technical */
        /* reasons. */
        /* BufferDetach(buffer, pool); */
      /* } */
      else {
        /* There is an active buffer, make sure it's nailed. */
        if(!amcSegHasNailboard(seg)) {
          if(SegNailed(seg) == TraceSetEMPTY) {
            res = amcSegCreateNailboard(seg, pool);
            if(res != ResOK) {
              /* Can't create nailboard, don't condemn. */
              return ResOK;
            }
            if(BufferScanLimit(buffer) != BufferLimit(buffer)) {
              NailboardSetRange(amcSegNailboard(seg),
                                BufferScanLimit(buffer),
                                BufferLimit(buffer));
            }
            ++trace->nailCount;
            SegSetNailed(seg, TraceSetSingle(trace));
          } else {
            /* Segment is nailed already, cannot create a nailboard */
            /* (see .nail.new), just give up condemning. */
            return ResOK;
          }
        } else {
          /* We have a nailboard, the buffer must be nailed already. */
          AVER(BufferScanLimit(buffer) == BufferLimit(buffer)
               || NailboardIsSetRange(amcSegNailboard(seg), 
                                      BufferScanLimit(buffer),
                                      BufferLimit(buffer)));
          /* Nail it for this trace as well. */
          SegSetNailed(seg, TraceSetAdd(SegNailed(seg), trace));
        }
        /* We didn't condemn the buffer, subtract it from the count. */
        /* @@@@ We could subtract all the nailed grains. */
        /* Relies on unsigned arithmetic wrapping round */
        /* on under- and overflow (which it does). */
        condemned -= AddrOffset(BufferScanLimit(buffer), BufferLimit(buffer));
      }
    }
  }

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  condemned += SegSize(seg);
  trace->condemned += condemned;

  gen = amcSegGen(seg);
  AVERT(amcGen, gen);
  if (!amcseg->old) {
    PoolGenAccountForAge(&gen->pgen, SegSize(seg), amcseg->deferred);
    amcseg->old = TRUE;
  }

  /* Ensure we are forwarding into the right generation. */

  /* see <design/poolamc/#gen.ramp> */
  /* This switching needs to be more complex for multiple traces. */
  AVER(TraceSetIsSingle(PoolArena(pool)->busyTraces));
  if(amc->rampMode == RampBEGIN && gen == amc->rampGen) {
    BufferDetach(gen->forward, pool);
    amcBufSetGen(gen->forward, gen);
    amc->rampMode = RampRAMPING;
  } else if(amc->rampMode == RampFINISH && gen == amc->rampGen) {
    BufferDetach(gen->forward, pool);
    amcBufSetGen(gen->forward, amc->afterRampGen);
    amc->rampMode = RampCOLLECTING;
  }

  return ResOK;
}


/* amcScanNailedRange -- make one scanning pass over a range of
 * addresses in a nailed segment.
 *
 * *totalReturn is set to FALSE if not all the objects between base and
 * limit have been scanned.  It is not touched otherwise.
 */
static Res amcScanNailedRange(Bool *totalReturn, Bool *moreReturn,
                              ScanState ss,
                              AMC amc, Nailboard board,
                              Addr base, Addr limit)
{
  Format format;
  Size headerSize;
  Addr p, clientLimit;
  Pool pool = MustBeA(AbstractPool, amc);
  format = pool->format;
  headerSize = format->headerSize;
  p = AddrAdd(base, headerSize);
  clientLimit = AddrAdd(limit, headerSize);
  while (p < clientLimit) {
    Addr q;
    q = (*format->skip)(p);
    if ((*amc->pinned)(amc, board, p, q)) {
      Res res = FormatScan(format, ss, p, q);
      if(res != ResOK) {
        *totalReturn = FALSE;
        *moreReturn = TRUE;
        return res;
      }
    } else {
      *totalReturn = FALSE;
    }
    AVER(p < q);
    p = q;
  }
  AVER(p == clientLimit);
  return ResOK;
}


/* amcScanNailedOnce -- make one scanning pass over a nailed segment
 *
 * *totalReturn is set to TRUE iff all objects in segment scanned.
 * *moreReturn is set to FALSE only if there are no more objects
 * on the segment that need scanning (which is normally the case).
 * It is set to TRUE if scanning had to be abandoned early on, and
 * also if during emergency fixing any new marks got added to the
 * nailboard.
 */
static Res amcScanNailedOnce(Bool *totalReturn, Bool *moreReturn,
                             ScanState ss, Seg seg, AMC amc)
{
  Addr p, limit;
  Nailboard board;
  Res res;

  EVENT3(AMCScanBegin, amc, seg, ss); /* TODO: consider using own event */

  *totalReturn = TRUE;
  board = amcSegNailboard(seg);
  NailboardClearNewNails(board);

  p = SegBase(seg);
  while(SegBuffer(seg) != NULL) {
    limit = BufferScanLimit(SegBuffer(seg));
    if(p >= limit) {
      AVER(p == limit);
      goto returnGood;
    }
    res = amcScanNailedRange(totalReturn, moreReturn,
                             ss, amc, board, p, limit);
    if (res != ResOK)
      return res;
    p = limit;
  }

  limit = SegLimit(seg);
  /* @@@@ Shouldn't p be set to BufferLimit here?! */
  res = amcScanNailedRange(totalReturn, moreReturn,
                           ss, amc, board, p, limit);
  if (res != ResOK)
    return res;

returnGood:
  EVENT3(AMCScanEnd, amc, seg, ss); /* TODO: consider using own event */

  *moreReturn = NailboardNewNails(board);
  return ResOK;
}


/* amcScanNailed -- scan a nailed segment */

static Res amcScanNailed(Bool *totalReturn, ScanState ss, Pool pool,
                         Seg seg, AMC amc)
{
  Bool total, moreScanning;
  size_t loops = 0;

  do {
    Res res;
    res = amcScanNailedOnce(&total, &moreScanning, ss, seg, amc);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    loops += 1;
  } while(moreScanning);

  if(loops > 1) {
    RefSet refset;

    AVER(ArenaEmergency(PoolArena(pool)));

    /* Looped: fixed refs (from 1st pass) were seen by MPS_FIX1
     * (in later passes), so the "ss.unfixedSummary" is _not_
     * purely unfixed.  In this one case, unfixedSummary is not 
     * accurate, and cannot be used to verify the SegSummary (see 
     * impl/trace/#verify.segsummary).  Use ScanStateSetSummary to 
     * store ScanStateSummary in ss.fixedSummary and reset 
     * ss.unfixedSummary.  See job001548.
     */
  
    refset = ScanStateSummary(ss);

    /* A rare event, which might prompt a rare defect to appear. */
    EVENT6(amcScanNailed, loops, SegSummary(seg), ScanStateWhite(ss), 
           ScanStateUnfixedSummary(ss), ss->fixedSummary, refset);
  
    ScanStateSetSummary(ss, refset);
  }
  
  *totalReturn = total;
  return ResOK;
}


/* AMCScan -- scan a single seg, turning it black
 *
 * See <design/poolamc/#seg-scan>.
 */
static Res AMCScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Addr base, limit;
  Format format;
  AMC amc = MustBeA(AMCZPool, pool);
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);


  format = pool->format;

  if(amcSegHasNailboard(seg)) {
    return amcScanNailed(totalReturn, ss, pool, seg, amc);
  }

  EVENT3(AMCScanBegin, amc, seg, ss);

  base = AddrAdd(SegBase(seg), format->headerSize);
  /* <design/poolamc/#seg-scan.loop> */
  while(SegBuffer(seg) != NULL) {
    limit = AddrAdd(BufferScanLimit(SegBuffer(seg)),
                    format->headerSize);
    if(base >= limit) {
      /* @@@@ Are we sure we don't need scan the rest of the */
      /* segment? */
      AVER(base == limit);
      *totalReturn = TRUE;
      return ResOK;
    }
    res = FormatScan(format, ss, base, limit);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    base = limit;
  }

  /* <design/poolamc/#seg-scan.finish> @@@@ base? */
  limit = AddrAdd(SegLimit(seg), format->headerSize);
  AVER(SegBase(seg) <= base);
  AVER(base <= AddrAdd(SegLimit(seg), format->headerSize));
  if(base < limit) {
    res = FormatScan(format, ss, base, limit);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  }

  EVENT3(AMCScanEnd, amc, seg, ss);

  *totalReturn = TRUE;
  return ResOK;
}


/* amcFixInPlace -- fix an reference without moving the object
 *
 * Usually this function is used for ambiguous references, but during
 * emergency tracing may be used for references of any rank.
 *
 * If the segment has a nailboard then we use that to record the fix.
 * Otherwise we simply grey and nail the entire segment.
 */
static void amcFixInPlace(Pool pool, Seg seg, ScanState ss, Ref *refIO)
{
  Addr ref;

  UNUSED(pool);

  ref = (Addr)*refIO;
  /* An ambiguous reference can point before the header. */
  AVER(SegBase(seg) <= ref);
  /* .ref-limit: A reference passed to Fix can't be beyond the */
  /* segment, because then TraceFix would not have picked this */
  /* segment. */
  AVER(ref < SegLimit(seg));

  EVENT0(AMCFixInPlace);
  if(amcSegHasNailboard(seg)) {
    Bool wasMarked = NailboardSet(amcSegNailboard(seg), ref);
    /* If there are no new marks (i.e., no new traces for which we */
    /* are marking, and no new mark bits set) then we can return */
    /* immediately, without changing colour. */
    if(TraceSetSub(ss->traces, SegNailed(seg)) && wasMarked)
      return;
  } else if(TraceSetSub(ss->traces, SegNailed(seg))) {
    return;
  }
  SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
  /* AMCZ segments don't contain references and so don't need to */
  /* become grey */
  if(SegRankSet(seg) != RankSetEMPTY)
    SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
}


/* AMCFixEmergency -- fix a reference, without allocating
 *
 * See <design/poolamc/#emergency.fix>.
 */
static Res AMCFixEmergency(Pool pool, ScanState ss, Seg seg,
                           Ref *refIO)
{
  Arena arena;
  Addr newRef;

  AVERC(AMCZPool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);

  arena = PoolArena(pool);

  ss->wasMarked = TRUE;

  if(ss->rank == RankAMBIG)
    goto fixInPlace;

  ShieldExpose(arena, seg);
  newRef = (*pool->format->isMoved)(*refIO);
  ShieldCover(arena, seg);
  if(newRef != (Addr)0) {
    /* Object has been forwarded already, so snap-out pointer. */
    /* TODO: Implement weak pointer semantics in emergency fixing.  This
       would be a good idea since we really want to reclaim as much as
       possible in an emergency. */
    *refIO = newRef;
    return ResOK;
  }

fixInPlace: /* see <design/poolamc/>.Nailboard.emergency */
  amcFixInPlace(pool, seg, ss, refIO);
  return ResOK;
}


/* AMCFix -- fix a reference to the pool
 *
 * See <design/poolamc/#fix>.
 */
static Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Res res;
  Format format;       /* cache of pool->format */
  Size headerSize;     /* cache of pool->format->headerSize */
  Ref ref;             /* reference to be fixed */
  Addr base;           /* base address of reference */
  Ref newRef;          /* new location, if moved */
  Addr newBase;        /* base address of new copy */
  Size length;         /* length of object to be relocated */
  Buffer buffer;       /* buffer to allocate new copy into */
  amcGen gen;          /* generation of old copy of object */
  TraceSet grey;       /* greyness of object being relocated */
  Seg toSeg;           /* segment to which object is being relocated */

  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);
  EVENT0(AMCFix);

  /* For the moment, assume that the object was already marked. */
  /* (See <design/fix/#protocol.was-marked>.) */
  ss->wasMarked = TRUE;

  /* If the reference is ambiguous, set up the datastructures for */
  /* managing a nailed segment.  This involves marking the segment */
  /* as nailed, and setting up a per-word mark table */
  if(ss->rank == RankAMBIG) {
    /* .nail.new: Check to see whether we need a Nailboard for */
    /* this seg.  We use "SegNailed(seg) == TraceSetEMPTY" */
    /* rather than "!amcSegHasNailboard(seg)" because this avoids */
    /* setting up a new nailboard when the segment was nailed, but */
    /* had no nailboard.  This must be avoided because otherwise */
    /* assumptions in AMCFixEmergency will be wrong (essentially */
    /* we will lose some pointer fixes because we introduced a */
    /* nailboard). */
    if(SegNailed(seg) == TraceSetEMPTY) {
      res = amcSegCreateNailboard(seg, pool);
      if(res != ResOK)
        return res;
      ++ss->nailCount;
      SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
    }
    amcFixInPlace(pool, seg, ss, refIO);
    return ResOK;
  }

  amc = MustBeA_CRITICAL(AMCZPool, pool);
  AVERT_CRITICAL(AMC, amc);
  format = pool->format;
  headerSize = format->headerSize;
  ref = *refIO;
  AVER_CRITICAL(AddrAdd(SegBase(seg), headerSize) <= ref);
  base = AddrSub(ref, headerSize);
  AVER_CRITICAL(AddrIsAligned(base, PoolAlignment(pool)));  
  AVER_CRITICAL(ref < SegLimit(seg)); /* see .ref-limit */
  arena = pool->arena;

  /* .exposed.seg: Statements tagged ".exposed.seg" below require */
  /* that "seg" (that is: the 'from' seg) has been ShieldExposed. */
  ShieldExpose(arena, seg);
  newRef = (*format->isMoved)(ref);  /* .exposed.seg */

  if(newRef == (Addr)0) {
    Addr clientQ;
    clientQ = (*format->skip)(ref);

    /* If object is nailed already then we mustn't copy it: */
    if (SegNailed(seg) != TraceSetEMPTY
        && !(amcSegHasNailboard(seg)
             && !(*amc->pinned)(amc, amcSegNailboard(seg), ref, clientQ)))
    {
      /* Segment only needs greying if there are new traces for */
      /* which we are nailing. */
      if(!TraceSetSub(ss->traces, SegNailed(seg))) {
        if(SegRankSet(seg) != RankSetEMPTY) /* not for AMCZ */
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
        SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
      }
      res = ResOK;
      goto returnRes;
    } else if(ss->rank == RankWEAK) {
      /* Object is not preserved (neither moved, nor nailed) */
      /* hence, reference should be splatted. */
      goto updateReference;
    }
    /* Object is not preserved yet (neither moved, nor nailed) */
    /* so should be preserved by forwarding. */

    /* <design/fix/#protocol.was-marked> */
    ss->wasMarked = FALSE;

    /* Get the forwarding buffer from the object's generation. */
    gen = amcSegGen(seg);
    buffer = gen->forward;
    AVER_CRITICAL(buffer != NULL);

    length = AddrOffset(ref, clientQ);  /* .exposed.seg */
    STATISTIC_STAT(++ss->forwardedCount);
    ss->forwardedSize += length;
    do {
      res = BUFFER_RESERVE(&newBase, buffer, length);
      if (res != ResOK)
        goto returnRes;
      newRef = AddrAdd(newBase, headerSize);

      toSeg = BufferSeg(buffer);
      ShieldExpose(arena, toSeg);

      /* Since we're moving an object from one segment to another, */
      /* union the greyness and the summaries together. */
      grey = SegGrey(seg);
      if(SegRankSet(seg) != RankSetEMPTY) { /* not for AMCZ */
        grey = TraceSetUnion(grey, ss->traces);
        SegSetSummary(toSeg, RefSetUnion(SegSummary(toSeg), SegSummary(seg)));
      } else {
        AVER(SegRankSet(toSeg) == RankSetEMPTY);
      }
      SegSetGrey(toSeg, TraceSetUnion(SegGrey(toSeg), grey));

      /* <design/trace/#fix.copy> */
      (void)AddrCopy(newBase, base, length);  /* .exposed.seg */

      ShieldCover(arena, toSeg);
    } while (!BUFFER_COMMIT(buffer, newBase, length));
    ss->copiedSize += length;

    (*format->move)(ref, newRef);  /* .exposed.seg */

    EVENT1(AMCFixForward, newRef);
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
  ShieldCover(arena, seg);  /* .exposed.seg */
  return res;
}


/* amcReclaimNailed -- reclaim what you can from a nailed segment */

static void amcReclaimNailed(Pool pool, Trace trace, Seg seg)
{
  Addr p, limit;
  Arena arena;
  Format format;
  Size bytesReclaimed = (Size)0;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  AMC amc = MustBeA(AMCZPool, pool);
  Size headerSize;
  Addr padBase;          /* base of next padding object */
  Size padLength;        /* length of next padding object */

  /* All arguments AVERed by AMCReclaim */

  format = pool->format;

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* see <design/poolamc/#nailboard.limitations> for improvements */
  headerSize = format->headerSize;
  ShieldExpose(arena, seg);
  p = SegBase(seg);
  if(SegBuffer(seg) != NULL) {
    limit = BufferScanLimit(SegBuffer(seg));
  } else {
    limit = SegLimit(seg);
  }
  padBase = p;
  padLength = 0;
  while(p < limit) {
    Addr clientP, q, clientQ;
    Size length;
    Bool preserve;
    clientP = AddrAdd(p, headerSize);
    clientQ = (*format->skip)(clientP);
    q = AddrSub(clientQ, headerSize);
    length = AddrOffset(p, q);
    if(amcSegHasNailboard(seg)) {
      preserve = (*amc->pinned)(amc, amcSegNailboard(seg), clientP, clientQ);
    } else {
      /* There's no nailboard, so preserve everything that hasn't been
       * forwarded. In this case, preservedInPlace* become somewhat
       * overstated. */
      preserve = !(*format->isMoved)(clientP);
    }
    if(preserve) {
      ++preservedInPlaceCount;
      preservedInPlaceSize += length;
      if (padLength > 0) {
        /* Replace run of forwarding pointers and unreachable objects
         * with a padding object. */
        (*format->pad)(padBase, padLength);
        bytesReclaimed += padLength;
        padLength = 0;
      }
      padBase = q;
    } else {
      padLength += length;
    }
    
    AVER(p < q);
    p = q;
  }
  AVER(p == limit);
  AVER(AddrAdd(padBase, padLength) == limit);
  if (padLength > 0) {
    /* Replace final run of forwarding pointers and unreachable
     * objects with a padding object. */
    (*format->pad)(padBase, padLength);
    bytesReclaimed += padLength;
  }
  ShieldCover(arena, seg);

  SegSetNailed(seg, TraceSetDel(SegNailed(seg), trace));
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  if(SegNailed(seg) == TraceSetEMPTY && amcSegHasNailboard(seg)) {
    NailboardDestroy(amcSegNailboard(seg), arena);
    Seg2amcSeg(seg)->board = NULL;
  }

  AVER(bytesReclaimed <= SegSize(seg));
  trace->reclaimSize += bytesReclaimed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;

  /* Free the seg if we can; fixes .nailboard.limitations.middle. */
  if(preservedInPlaceCount == 0
     && (SegBuffer(seg) == NULL)
     && (SegNailed(seg) == TraceSetEMPTY)) {

    amcGen gen = amcSegGen(seg);

    /* We may not free a buffered seg. */
    AVER(SegBuffer(seg) == NULL);

    PoolGenFree(&gen->pgen, seg, 0, SegSize(seg), 0, Seg2amcSeg(seg)->deferred);
  }
}


/* AMCReclaim -- recycle a segment if it is still white
 *
 * See <design/poolamc/#reclaim>.
 */
static void AMCReclaim(Pool pool, Trace trace, Seg seg)
{
  AMC amc = MustBeA_CRITICAL(AMCZPool, pool);
  amcGen gen;

  AVERT_CRITICAL(Trace, trace);
  AVERT_CRITICAL(Seg, seg);

  gen = amcSegGen(seg);
  AVERT_CRITICAL(amcGen, gen);

  EVENT3(AMCReclaim, gen, trace, seg);

  /* This switching needs to be more complex for multiple traces. */
  AVER_CRITICAL(TraceSetIsSingle(PoolArena(pool)->busyTraces));
  if(amc->rampMode == RampCOLLECTING) {
    if(amc->rampCount > 0) {
      /* Entered ramp mode before previous one was cleaned up */
      amc->rampMode = RampBEGIN;
    } else {
      amc->rampMode = RampOUTSIDE;
    }
  }

  if(SegNailed(seg) != TraceSetEMPTY) {
    amcReclaimNailed(pool, trace, seg);
    return;
  }

  /* We may not free a buffered seg.  (But all buffered + condemned */
  /* segs should have been nailed anyway). */
  AVER(SegBuffer(seg) == NULL);

  trace->reclaimSize += SegSize(seg);

  PoolGenFree(&gen->pgen, seg, 0, SegSize(seg), 0, Seg2amcSeg(seg)->deferred);
}


/* AMCWalk -- Apply function to (black) objects in segment */

static void AMCWalk(Pool pool, Seg seg, FormattedObjectsVisitor f,
                    void *p, size_t s)
{
  Addr object, nextObject, limit;
  Format format;

  AVERC(AMCZPool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures so can't be checked */

  /* Avoid applying the function to grey or white objects. */
  /* White objects might not be alive, and grey objects */
  /* may have pointers to old-space. */

  /* NB, segments containing a mix of colours (i.e., nailed segs) */
  /* are not handled properly:  No objects are walked.  See */
  /* job001682. */
  if(SegWhite(seg) == TraceSetEMPTY && SegGrey(seg) == TraceSetEMPTY
     && SegNailed(seg) == TraceSetEMPTY)
  {
    format = pool->format;

    /* If the segment is buffered, only walk as far as the end */
    /* of the initialized objects.  cf. AMCScan */
    if(SegBuffer(seg) != NULL)
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


/* amcWalkAll -- Apply a function to all (black) objects in a pool */

static void amcWalkAll(Pool pool, FormattedObjectsVisitor f, void *p, size_t s)
{
  Arena arena;
  Ring ring, next, node;

  AVER(IsA(AMCZPool, pool));

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


/* amcAddrObjectSearch -- skip over objects (belonging to pool)
 * starting at objBase until we reach one of the following cases:
 * 1. addr is found (and not moved): set *pReturn to the client
 * pointer to the object containing addr and return ResOK;
 * 2. addr is found, but it moved: return ResFAIL;
 * 3. we reach searchLimit: return ResFAIL.
 */
static Res amcAddrObjectSearch(Addr *pReturn, Pool pool, Addr objBase,
                               Addr searchLimit, Addr addr)
{
  Format format;
  Size hdrSize;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(objBase <= searchLimit);

  format = pool->format;
  hdrSize = format->headerSize;
  while (objBase < searchLimit) {
    Addr objRef = AddrAdd(objBase, hdrSize);
    Addr objLimit = AddrSub((*format->skip)(objRef), hdrSize);
    AVER(objBase < objLimit);
    if (addr < objLimit) {
      AVER(objBase <= addr);
      AVER(addr < objLimit); /* the point */
      if (!(*format->isMoved)(objRef)) {
        *pReturn = objRef;
        return ResOK;
      }
      break;
    }
    objBase = objLimit;
  }
  return ResFAIL;
}


/* AMCAddrObject -- find client pointer to object containing addr.
 * addr is known to belong to seg, which belongs to pool.
 * See job003589.
 */
static Res AMCAddrObject(Addr *pReturn, Pool pool, Seg seg, Addr addr)
{
  Res res;
  Arena arena;
  Addr base, limit;    /* range of objects on segment */

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegPool(seg) == pool);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));

  arena = PoolArena(pool);
  base = SegBase(seg);
  if (SegBuffer(seg) != NULL) {
    /* We use BufferGetInit here (and not BufferScanLimit) because we
     * want to be able to find objects that have been allocated and
     * committed since the last flip. These objects lie between the
     * addresses returned by BufferScanLimit (which returns the value
     * of init at the last flip) and BufferGetInit.
     *
     * Strictly speaking we only need a limit that is at least the
     * maximum of the objects on the segments. This is because addr
     * *must* point inside a live object and we stop skipping once we
     * have found it. The init pointer serves this purpose.
     */
    limit = BufferGetInit(SegBuffer(seg));
  } else {
    limit = SegLimit(seg);
  }

  ShieldExpose(arena, seg);
  res = amcAddrObjectSearch(pReturn, pool, base, limit, addr);
  ShieldCover(arena, seg);
  return res;
}


/* AMCTotalSize -- total memory allocated from the arena */

static Size AMCTotalSize(Pool pool)
{
  AMC amc = MustBeA(AMCZPool, pool);
  Size size = 0;
  Ring node, nextNode;

  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    AVERT(amcGen, gen);
    size += gen->pgen.totalSize;
  }

  return size;
}


/* AMCFreeSize -- free memory (unused by client program) */

static Size AMCFreeSize(Pool pool)
{
  AMC amc = MustBeA(AMCZPool, pool);
  Size size = 0;
  Ring node, nextNode;

  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    AVERT(amcGen, gen);
    size += gen->pgen.freeSize;
  }

  return size;
}


/* AMCDescribe -- describe the contents of the AMC pool
 *
 * See <design/poolamc/#describe>.
 */
static Res AMCDescribe(Pool pool, mps_lib_FILE *stream, Count depth)
{
  Res res;
  AMC amc = CouldBeA(AMCZPool, pool);
  Ring node, nextNode;
  const char *rampmode;

  if(!TESTC(AMCZPool, amc))
    return ResPARAM;
  if(stream == NULL)
    return ResPARAM;

  res = WriteF(stream, depth,
               (amc->rankSet == RankSetEMPTY) ? "AMCZ" : "AMC",
               " $P {\n", (WriteFP)amc, "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               NULL);
  if(res != ResOK)
    return res;

  switch(amc->rampMode) {
#define RAMP_DESCRIBE(e, s)     \
    case e:                     \
      rampmode = s;             \
      break;
    RAMP_RELATION(RAMP_DESCRIBE)
#undef RAMP_DESCRIBE
    default:
      rampmode = "unknown ramp mode";
      break;
  }
  res = WriteF(stream, depth + 2,
               rampmode, " ($U)\n", (WriteFU)amc->rampCount,
               NULL);
  if(res != ResOK)
    return res;

  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    res = amcGenDescribe(gen, stream, depth + 2);
    if(res != ResOK)
      return res;
  }

  if(0) {
    /* SegDescribes */
    RING_FOR(node, &pool->segRing, nextNode) {
      Seg seg = RING_ELT(Seg, poolRing, node);
      res = AMCSegDescribe(seg, stream, depth + 2);
      if(res != ResOK)
        return res;
    }
  }

  res = WriteF(stream, depth, "} AMC $P\n", (WriteFP)amc, NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* AMCZPoolClass -- the class definition */

DEFINE_CLASS(Pool, AMCZPool, this)
{
  INHERIT_CLASS(this, AMCZPool, AbstractSegBufPool);
  PoolClassMixInFormat(this);
  PoolClassMixInCollect(this);
  this->size = sizeof(AMCStruct);
  this->attr |= AttrMOVINGGC;
  this->varargs = AMCVarargs;
  this->init = AMCZInit;
  this->finish = AMCFinish;
  this->bufferFill = AMCBufferFill;
  this->bufferEmpty = AMCBufferEmpty;
  this->whiten = AMCWhiten;
  this->fix = AMCFix;
  this->fixEmergency = AMCFixEmergency;
  this->reclaim = AMCReclaim;
  this->rampBegin = AMCRampBegin;
  this->rampEnd = AMCRampEnd;
  this->addrObject = AMCAddrObject;
  this->walk = AMCWalk;
  this->bufferClass = amcBufClassGet;
  this->totalSize = AMCTotalSize;
  this->freeSize = AMCFreeSize;  
  this->describe = AMCDescribe;
}


/* AMCPoolClass -- the class definition */

DEFINE_CLASS(Pool, AMCPool, this)
{
  INHERIT_CLASS(this, AMCPool, AMCZPool);
  PoolClassMixInScan(this);
  this->init = AMCInit;
  this->scan = AMCScan;
}


/* mps_class_amc -- return the pool class descriptor to the client */

mps_pool_class_t mps_class_amc(void)
{
  return (mps_pool_class_t)CLASS(AMCPool);
}

/* mps_class_amcz -- return the pool class descriptor to the client */

mps_pool_class_t mps_class_amcz(void)
{
  return (mps_pool_class_t)CLASS(AMCZPool);
}


/* mps_amc_apply -- apply function to all objects in pool
 *
 * The iterator that is passed by the client is stored in a closure
 * structure which is passed to a local iterator in order to ensure
 * that any type conversion necessary between Addr and mps_addr_t
 * happen. They are almost certainly the same on all platforms, but 
 * this is the correct way to do it.
*/

typedef struct mps_amc_apply_closure_s {
  mps_amc_apply_stepper_t f;
  void *p;
  size_t s;
} mps_amc_apply_closure_s;

static void mps_amc_apply_iter(Addr addr, Format format, Pool pool,
                               void *p, size_t s)
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
                   mps_amc_apply_stepper_t f,
                   void *p, size_t s)
{
  Pool pool = (Pool)mps_pool;
  mps_amc_apply_closure_s closure_s;
  Arena arena;

  AVER(TESTT(Pool, pool));
  arena = PoolArena(pool);
  ArenaEnter(arena);
  AVERT(Pool, pool);

  closure_s.f = f;
  closure_s.p = p;
  closure_s.s = s;
  amcWalkAll(pool, mps_amc_apply_iter, &closure_s, sizeof(closure_s));

  ArenaLeave(arena);
}


/* AMCCheck -- check consistency of the AMC pool
 *
 * See <design/poolamc/#check>.
 */

ATTRIBUTE_UNUSED
static Bool AMCCheck(AMC amc)
{
  CHECKS(AMC, amc);
  CHECKC(AMCZPool, amc);
  CHECKD(Pool, MustBeA(AbstractPool, amc));
  CHECKL(RankSetCheck(amc->rankSet));
  CHECKD_NOSIG(Ring, &amc->genRing);
  CHECKL(BoolCheck(amc->gensBooted));
  if(amc->gensBooted) {
    CHECKD(amcGen, amc->nursery);
    CHECKL(amc->gen != NULL);
    CHECKD(amcGen, amc->rampGen);
    CHECKD(amcGen, amc->afterRampGen);
  }

  CHECKL(amc->rampMode >= RampOUTSIDE);
  CHECKL(amc->rampMode <= RampCOLLECTING);

  /* if OUTSIDE, count must be zero. */
  CHECKL((amc->rampCount == 0) || (amc->rampMode != RampOUTSIDE));
  /* if BEGIN or RAMPING, count must not be zero. */
  CHECKL((amc->rampCount != 0) || ((amc->rampMode != RampBEGIN) &&
                                   (amc->rampMode != RampRAMPING)));

  return TRUE;
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

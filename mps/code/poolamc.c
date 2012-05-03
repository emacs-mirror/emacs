/* poolamc.c: AUTOMATIC MOSTLY-COPYING MEMORY POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .sources: <design/poolamc/>.
 */

#include "mpscamc.h"
#include "chain.h"
#include "bt.h"
#include "mpm.h"

SRCID(poolamc, "$Id$");

/* PType enumeration -- distinguishes AMCGen and AMCNailboard */
enum {AMCPTypeGen = 1, AMCPTypeNailboard};

/* AMC typedef */
typedef struct AMCStruct *AMC;

/* amcGen typedef */
typedef struct amcGenStruct *amcGen;

/* forward declarations */

static Bool amcSegHasNailboard(Seg seg);
static Bool AMCCheck(AMC amc);
static Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
static Res AMCHeaderFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
static PoolClass AMCPoolClassGet(void);
static BufferClass amcBufClassGet(void);
static SegClass amcSegClassGet(void);


/* amcGenStruct -- pool AMC generation descriptor */

#define amcGenSig       ((Sig)0x519A3C9E)  /* SIGnature AMC GEn */

typedef struct amcGenStruct {
  PoolGenStruct pgen;
  int type;                     /* AMCPTypeGen for a gen */
  RingStruct amcRing;           /* link in list of gens in pool */
  Buffer forward;               /* forwarding buffer */
  Count segs;                   /* number of segs in gen */
  Sig sig;                      /* <code/misc.h#sig> */
} amcGenStruct;

#define amcGenAMC(amcgen) Pool2AMC((amcgen)->pgen.pool)
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


/* amcNailboard -- the nailboard */

typedef struct amcNailboardStruct *amcNailboard;
typedef struct amcNailboardStruct {
  Sig sig;
  int type;         /* AMCPTypeNailboard for a nailboard */
  amcGen gen;       /* generation of this segment */
  Count nails;      /* no. of ambigFixes, not necessarily distinct */
  Count distinctNails; /* number of distinct ambigFixes */
  Bool newMarks;    /* set to TRUE if a new mark bit is added */
  Shift markShift;  /* to convert offset into bit index for mark */
  BT mark;          /* mark table used to record ambiguous fixes */
} amcNailboardStruct;

#define amcNailboardSig ((Sig)0x519A3C4B) /* SIGnature AMC Nailboard */


/* amcSegStruct -- AMC-specific fields appended to GCSegStruct
 *
 * .segtype: logically, AMC segs should have pointers to: 
 *   - the generation (amcGenStruct);
 *   - the nailboard (or NULL if not present).
 * But in fact (apparently to save space in the amcSegStruct?) these 
 * pointers are encoded, so as to use only a single-word "segTypeP" 
 * field in amcSegStruct, as follows:
 * The "segTypeP" field is a pointer to (the type field of) either
 * a nailboard or a generation.  The value stored in the type field 
 * indicates whether its enclosing struct is a generation or a 
 * nailboard.  The segTypeP field is initialised by passing an 
 * additional parameter (the address of the segment's generation) to 
 * SegAlloc.  See <design/poolamc/#fix.nail.distinguish>.
 *
 * .seg-ramp-new: "new" (if true) means this segment was allocated by 
 * AMCBufferFill while amc->rampMode == RampRAMPING, and therefore 
 * (I think) the contribution it *should* make to gen->pgen.newSize
 * is being deferred until the ramping is over.  "new" is set to FALSE
 * in all other (ie. normal) circumstances.  (The original comment for 
 * this struct member was "allocated since last GC").  RHSK 2009-04-15.
 */

typedef struct amcSegStruct *amcSeg;

#define amcSegSig      ((Sig)0x519A3C59) /* SIGnature AMC SeG */

typedef struct amcSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  int *segTypeP;            /* .segtype */
  Bool new;                 /* .seg-ramp-new */
  Sig sig;                  /* <code/misc.h#sig> */
} amcSegStruct;

#define Seg2amcSeg(seg)             ((amcSeg)(seg))
#define amcSeg2Seg(amcseg)          ((Seg)(amcseg))

#define amcSegTypeP(seg)           (Seg2amcSeg(seg)->segTypeP)
#define amcSegSetTypeP(seg, type)  (Seg2amcSeg(seg)->segTypeP = (type))


static Bool amcSegCheck(amcSeg amcseg)
{
  CHECKS(amcSeg, amcseg);
  CHECKD(GCSeg, &amcseg->gcSegStruct);
  CHECKL(*amcseg->segTypeP == AMCPTypeNailboard
         || *amcseg->segTypeP == AMCPTypeGen);
  if(*amcseg->segTypeP == AMCPTypeNailboard) {
    CHECKL(SegNailed(amcSeg2Seg(amcseg)) != TraceSetEMPTY);
  }
  CHECKL(BoolCheck(amcseg->new));
  return TRUE;
}


/* AMCSegInit -- initialise an AMC segment */

static Res AMCSegInit(Seg seg, Pool pool, Addr base, Size size,
                      Bool reservoirPermit, va_list args)
{
  int *segtype = va_arg(args, int*);  /* .segtype */
  SegClass super;
  amcSeg amcseg;
  Res res;

  AVERT(Seg, seg);
  amcseg = Seg2amcSeg(seg);
  /* no useful checks for base and size */
  AVERT(Bool, reservoirPermit);

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(amcSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if(res != ResOK)
    return res;

  amcseg->segTypeP = segtype; /* .segtype */
  amcseg->new = TRUE;
  amcseg->sig = amcSegSig;
  AVERT(amcSeg, amcseg);

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
    Bool limitzeroed = (buffer->apStruct.limit == 0);

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
static Res AMCSegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;
  Pool pool;
  amcSeg amcseg;
  SegClass super;
  Addr i, p, base, limit, init;
  Align step;
  Size row;
  char abzSketch[5];

  if(!CHECKT(Seg, seg))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;
  amcseg = Seg2amcSeg(seg);
  if(!CHECKT(amcSeg, amcseg))
    return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(amcSegClass);
  res = super->describe(seg, stream);
  if(res != ResOK)
    return res;

  pool = SegPool(seg);
  step = PoolAlignment(pool);
  row = step * 64;

  base = SegBase(seg);
  p = AddrAdd(base, pool->format->headerSize);
  limit = SegLimit(seg);

  res = WriteF(stream,
               "AMC seg $P [$A,$A){\n",
               (WriteFP)seg, (WriteFA)base, (WriteFA)limit,
               NULL);
  if(res != ResOK)
    return res;

  if(amcSegHasNailboard(seg)) {
    res = WriteF(stream, "  Boarded\n", NULL);
    /* @@@@ should have AMCNailboardDescribe() */
  } else {
    if(SegNailed(seg) == TraceSetEMPTY) {
      res = WriteF(stream, "  Mobile\n", NULL);
    } else {
      res = WriteF(stream, "  Stuck\n", NULL);
    }
  }
  if(res != ResOK)
    return res;

  res = WriteF(stream, "  Map:  *===:object  bbbb:buffer\n", NULL);
  if(res != ResOK)
    return res;

  if(SegBuffer(seg) != NULL)
    init = BufferGetInit(SegBuffer(seg));
  else
    init = limit;
  
  for(i = base; i < limit; i = AddrAdd(i, row)) {
    Addr j;
    char c;

    res = WriteF(stream, "    $A  ", i, NULL);
    if(res != ResOK)
      return res;

    /* @@@@ This needs to describe nailboards as well */
    /* @@@@ This misses a header-sized pad at the end. */
    for(j = i; j < AddrAdd(i, row); j = AddrAdd(j, step)) {
      if(j >= limit)
        c = ' ';  /* if seg is not a whole number of print rows */
      else if(j >= init)
        c = 'b';
      else if(j == p) {
        c = '*';
        p = (pool->format->skip)(p);
      } else {
        c = '=';
      }
      res = WriteF(stream, "$C", c, NULL);
      if(res != ResOK)
        return res;
    }

    res = WriteF(stream, "\n", NULL);
    if(res != ResOK)
      return res;
  }

  AMCSegSketch(seg, abzSketch, NELEMS(abzSketch));
  res = WriteF(stream, "  Sketch: $S\n", (WriteFS)abzSketch, NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream, "} AMC Seg $P\n", (WriteFP)seg, NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* amcSegClass -- Class definition for AMC segments */

DEFINE_SEG_CLASS(amcSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this (yet) */
  class->name = "AMCSEG";
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
  int type;

  type = *amcSegTypeP(seg);
  AVER(type == AMCPTypeNailboard || type == AMCPTypeGen);
  return type == AMCPTypeNailboard;
}


/* amcSegNailboard -- get the nailboard for this segment */

static amcNailboard amcSegNailboard(Seg seg)
{
  int *p;

  p = amcSegTypeP(seg);
  AVER(amcSegHasNailboard(seg));
  return PARENT(amcNailboardStruct, type, p);
}


/* amcSegGen -- get the generation structure for this segment */

static amcGen amcSegGen(Seg seg)
{
  if(amcSegHasNailboard(seg)) {
    amcNailboard Nailboard = amcSegNailboard(seg);
    return Nailboard->gen;
  } else {
    int *p;
    p = amcSegTypeP(seg);
    return PARENT(amcGenStruct, type, p);
  }
}


/* AMCStruct -- pool AMC descriptor
 *
 * See <design/poolamc/#struct>.
 */

#define AMCSig          ((Sig)0x519A3C99) /* SIGnature AMC */

typedef struct PageRetStruct {
  Count pCond;     /* pages Condemned */
  Count pRet;      /* pages Retained (in place) */
  /* Small */
  Count pCS;       /* pages Condemned in Small segments */
  Count pRS;       /* pages Retained in Small segments */
  /* Medium */
  Count sCM;       /* segments Condemned: Medium */
                   /* ...= upper bound of how many extra pages it */
                   /*    would have cost, had we chosen to LSP-pad */
                   /*    all these segments. */
  Count pCM;       /* pages Condemned in Medium segments */
  Count sRM;       /* segments Retained: Medium */
  Count pRM;       /* pages Retained in Medium segments: */
  Count pRM1;      /*   ...because obj 1 was preserved in place */
                   /*   ...because a rest obj was pip, causing: */
  Count pRMrr;     /*     ...retained rest pages (page where rest obj is) */
  Count pRMr1;     /*     ...retained obj 1 pages (purely NMR pad) */
  /* Large */
  Count sCL;       /* segments Condemned: Large */
                   /* ...= upper bound of how many extra pages it */
                   /*    has cost to LSP-pad all these segments. */
  Count pCL;       /* pages Condemned in Large segments */
  Count sRL;       /* segments Retained: Large */
  Count pRL;       /* pages Retained in Large segments */
  Count pRLr;      /*   ...because a rest obj (actually LSP) was pip */

  /* The interesting things about this report are:
   *   - How many pages are actually being retained? (pRet)
   *   - Percentage? (pRet/pCond)
   *   - Is the major contribution from Small, Medium, or Large segs?
   *
   * Generally, pages retained because obj 1 needed to be preserved in 
   * place are ok (because no alternative placement could have retained 
   * fewer pages), but pages retained by a rest obj are unfortunate 
   * (better placement, putting the small rest objs in their own seg, 
   * would have retained fewer pages).  In particular:
   *
   * The LSP threshold is a payoff between the wasted space from 
   * LSP-padding, versus the risk of increased page-retention (due to 
   * rest objs) from not LSP-padding.
   *
   * For Medium segs, where we do not do LSP-padding:
   *   - LSP would have required at most sCM extra pages;
   *   - the extra retention incurred by not LSP-padding is pRMr1.
   * A high pRMr1 => lots of Medium segs getting retained by the rest 
   * objs tacked on after obj 1.  Consider lowering LSP-threshold.
   *
   * For Large segs we do LSP padding.  This has a cost; upper bound is 
   * sCL extra pages.  But the benefit should be greatly reduced ambig 
   * refs to rest objs.  With LSP, the only rest obj is the LSP pad 
   * itself.  We expect that ambig refs to this are rare, so currently 
   * we do not implement .large.lsp-no-retain.  But we do record the 
   * occurrence of pages retained by a ref to an LSP pad: pPLr.  A high 
   * pRLr => perhaps .large.lsp-no-retain should be implemented?
   *
   * If the mutator is causing a lot of page retention, then sRM/pRM
   * and sRL/pRL should give some picture of the number of retained 
   * objects and their average size.
   */
} PageRetStruct;

/* static => init'd to zero */
static struct PageRetStruct pageretstruct_Zero;

typedef struct AMCStruct { /* <design/poolamc/#struct> */
  PoolStruct poolStruct;   /* generic pool structure */
  RankSet rankSet;         /* rankSet for entire pool */
  RingStruct genRing;      /* ring of generations */
  Bool gensBooted;         /* used during boot (init) */
  Chain chain;             /* chain used by this pool */
  size_t gens;             /* number of generations */
  amcGen *gen;             /* (pointer to) array of generations */
  amcGen nursery;          /* the default mutator generation */
  amcGen rampGen;          /* the ramp generation */
  amcGen afterRampGen;     /* the generation after rampGen */
  unsigned rampCount;      /* <design/poolamc/#ramp.count> */
  int rampMode;            /* <design/poolamc/#ramp.mode> */

  /* page retention in an in-progress trace */
  STATISTIC_DECL(PageRetStruct pageretstruct[TraceLIMIT]);

  Sig sig;                 /* <design/pool/#outer-structure.sig> */
} AMCStruct;

#define Pool2AMC(pool) PARENT(AMCStruct, poolStruct, (pool))
#define AMC2Pool(amc) (&(amc)->poolStruct)


/* amcGenCheck -- check consistency of a generation structure */

static Bool amcGenCheck(amcGen gen)
{
  Arena arena;
  AMC amc;

  CHECKS(amcGen, gen);
  CHECKD(PoolGen, &gen->pgen);
  amc = amcGenAMC(gen);
  CHECKU(AMC, amc);
  CHECKL(gen->type == AMCPTypeGen);
  CHECKD(Buffer, gen->forward);
  CHECKL(RingCheck(&gen->amcRing));
  CHECKL((gen->pgen.totalSize == 0) == (gen->segs == 0));
  arena = amc->poolStruct.arena;
  CHECKL(gen->pgen.totalSize >= gen->segs * ArenaAlign(arena));
  return TRUE;
}


/* amcNailboardCheck -- check the nailboard */

static Bool amcNailboardCheck(amcNailboard board)
{
  CHECKS(amcNailboard, board);
  CHECKL(board->type == AMCPTypeNailboard);
  CHECKD(amcGen, board->gen);
  /* nails is >= number of set bits in mark, but we can't check this */
  /* We know that shift corresponds to pool->align. */
  CHECKL(BoolCheck(board->newMarks));
  CHECKL(board->distinctNails <= board->nails);
  CHECKL((Align)1 << board->markShift
         == PoolAlignment(amcGenPool(board->gen)));
  /* weak check for BTs @@@@ */
  CHECKL(board->mark != NULL);
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
  Sig sig;                      /* <design/sig/> */
} amcBufStruct;


/* Buffer2amcBuf -- convert generic Buffer to an amcBuf */

#define Buffer2amcBuf(buffer) ((amcBuf)(buffer))



/* amcBufCheck -- check consistency of an amcBuf */

static Bool amcBufCheck(amcBuf amcbuf)
{
  SegBuf segbuf;

  CHECKS(amcBuf, amcbuf);
  segbuf = &amcbuf->segbufStruct;
  CHECKL(SegBufCheck(segbuf));
  if(amcbuf->gen != NULL)
    CHECKD(amcGen, amcbuf->gen);
  return TRUE;
}


/* amcBufGen -- Return the AMC generation of an amcBuf */

static amcGen amcBufGen(Buffer buffer)
{
  return Buffer2amcBuf(buffer)->gen;
}


/* amcBufSetGen -- Set the AMC generation of an amcBuf */

static void amcBufSetGen(Buffer buffer, amcGen gen)
{
  amcBuf amcbuf;

  if(gen != NULL)
    AVERT(amcGen, gen);
  amcbuf = Buffer2amcBuf(buffer);
  amcbuf->gen = gen;
}


/* AMCBufInit -- Initialize an amcBuf */

static Res AMCBufInit(Buffer buffer, Pool pool, va_list args)
{
  AMC amc;
  amcBuf amcbuf;
  BufferClass superclass;
  Res res;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);

  /* call next method */
  superclass = BUFFER_SUPERCLASS(amcBufClass);
  res = (*superclass->init)(buffer, pool, args);
  if(res != ResOK)
    return res;

  amcbuf = Buffer2amcBuf(buffer);
  if(BufferIsMutator(buffer)) {
    /* Set up the buffer to be allocating in the nursery. */
    amcbuf->gen = amc->nursery;
  } else {
    /* No gen yet -- see <design/poolamc/#gen.forward>. */
    amcbuf->gen = NULL;
  }
  amcbuf->sig = amcBufSig;
  AVERT(amcBuf, amcbuf);

  BufferSetRankSet(buffer, amc->rankSet);

  return ResOK;
}


/* AMCBufFinish -- Finish an amcBuf */

static void AMCBufFinish(Buffer buffer)
{
  BufferClass super;
  amcBuf amcbuf;

  AVERT(Buffer, buffer);
  amcbuf = Buffer2amcBuf(buffer);
  AVERT(amcBuf, amcbuf);

  amcbuf->sig = SigInvalid;

  /* Finish the superclass fields last. */
  super = BUFFER_SUPERCLASS(amcBufClass);
  super->finish(buffer);
}


/* amcBufClass -- The class definition */

DEFINE_BUFFER_CLASS(amcBufClass, class)
{
  INHERIT_CLASS(class, SegBufClass);
  class->name = "AMCBUF";
  class->size = sizeof(amcBufStruct);
  class->init = AMCBufInit;
  class->finish = AMCBufFinish;
}


/* amcGenCreate -- create a generation */

static Res amcGenCreate(amcGen *genReturn, AMC amc, Serial genNr)
{
  Arena arena;
  Buffer buffer;
  Pool pool;
  amcGen gen;
  Res res;
  void *p;

  pool = AMC2Pool(amc);
  arena = pool->arena;

  res = ControlAlloc(&p, arena, sizeof(amcGenStruct), FALSE);
  if(res != ResOK)
    goto failControlAlloc;
  gen = (amcGen)p;

  res = BufferCreate(&buffer, EnsureamcBufClass(), pool, FALSE);
  if(res != ResOK)
    goto failBufferCreate;

  res = PoolGenInit(&gen->pgen, amc->chain, genNr, pool);
  if(res != ResOK)
    goto failGenInit;
  gen->type = AMCPTypeGen;
  RingInit(&gen->amcRing);
  gen->segs = 0;
  gen->forward = buffer;
  gen->sig = amcGenSig;

  AVERT(amcGen, gen);

  RingAppend(&amc->genRing, &gen->amcRing);
  EVENT_PP(AMCGenCreate, amc, gen);
  *genReturn = gen;
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
  AVER(gen->segs == 0);
  AVER(gen->pgen.totalSize == 0);

  EVENT_P(AMCGenDestroy, gen);
  arena = PoolArena(amcGenPool(gen));
  gen->sig = SigInvalid;
  RingRemove(&gen->amcRing);
  RingFinish(&gen->amcRing);
  PoolGenFinish(&gen->pgen);
  BufferDestroy(gen->forward);
  ControlFree(arena, gen, sizeof(amcGenStruct));
}


/* amcGenDescribe -- describe an AMC generation */

static Res amcGenDescribe(amcGen gen, mps_lib_FILE *stream)
{
  Res res;

  if(!CHECKT(amcGen, gen))
    return ResFAIL;

  res = WriteF(stream,
               "  amcGen $P ($U) {\n",
               (WriteFP)gen, (WriteFU)amcGenNr(gen),
               "   buffer $P\n", gen->forward,
               "   segs $U, totalSize $U, newSize $U\n",
               (WriteFU)gen->segs,
               (WriteFU)gen->pgen.totalSize,
               (WriteFU)gen->pgen.newSize,
               "  } amcGen\n", NULL);
  return res;
}


/* amcSegCreateNailboard -- create nailboard for segment */

static Res amcSegCreateNailboard(Seg seg, Pool pool)
{
  amcNailboard board;
  Arena arena;
  Count bits;
  Res res;
  void *p;

  AVER(!amcSegHasNailboard(seg));

  arena = PoolArena(pool);

  res = ControlAlloc(&p, arena, sizeof(amcNailboardStruct), FALSE);
  if(res != ResOK)
    goto failAllocNailboard;
  board = p;
  board->type = AMCPTypeNailboard;
  board->gen = amcSegGen(seg);
  board->nails = (Count)0;
  board->distinctNails = (Count)0;
  board->newMarks = FALSE;
  board->markShift = SizeLog2((Size)pool->alignment);
  /* [I wonder what this comment is referring to?  2007-07-11 DRJ] */
  /* See d.m.p.Nailboard.size. */
  bits = (SegSize(seg) + pool->format->headerSize) >> board->markShift;
  res = ControlAlloc(&p, arena, BTSize(bits), FALSE);
  if(res != ResOK)
    goto failMarkTable;
  board->mark = p;
  BTResRange(board->mark, 0, bits);
  board->sig = amcNailboardSig;
  AVERT(amcNailboard, board);
  amcSegSetTypeP(seg, &board->type); /* .segtype */
  return ResOK;

failMarkTable:
  ControlFree(arena, board, sizeof(amcNailboardStruct));
failAllocNailboard:
  return res;
}


/* amcSegDestroyNailboard -- destroy the nailboard of a segment */

static void amcSegDestroyNailboard(Seg seg, Pool pool)
{
  amcNailboard board;
  amcGen gen;
  Arena arena;
  Count bits;

  gen = amcSegGen(seg);
  board = amcSegNailboard(seg);
  AVERT(amcNailboard, board);

  arena = PoolArena(pool);
  /* See d.m.p.Nailboard.size. */
  bits = (SegSize(seg) + pool->format->headerSize) >> board->markShift;
  ControlFree(arena, board->mark, BTSize(bits));
  board->sig = SigInvalid;
  ControlFree(arena, board, sizeof(amcNailboardStruct));
  amcSegSetTypeP(seg, &gen->type); /* .segtype */
}


/* amcNailGetMark -- get the mark bit for ref from the nailboard */

static Bool amcNailGetMark(Seg seg, Ref ref)
{
  amcNailboard board;
  Index i;

  board = amcSegNailboard(seg);
  AVERT(amcNailboard, board);

  i = AddrOffset(SegBase(seg), ref) >> board->markShift;
  return BTGet(board->mark, i);
}


/* amcNailGetAndSetMark -- set the mark bit for ref in the nailboard
 *
 * Returns the old value.
 */
static Bool amcNailGetAndSetMark(Seg seg, Ref ref)
{
  amcNailboard board;
  Index i;

  board = amcSegNailboard(seg);
  AVERT(amcNailboard, board);

  ++board->nails;
  i = AddrOffset(SegBase(seg), ref) >> board->markShift;
  if(!BTGet(board->mark, i)) {
    BTSet(board->mark, i);
    board->newMarks = TRUE;
    ++board->distinctNails;
    return FALSE;
  }
  return TRUE;
}


/* amcNailMarkRange -- nail a range in the board
 *
 * We nail the objects laying between base and limit, i.e., mark the
 * bits that correspond to client pointers for them.  We may assume
 * that the range is unmarked.
 */
static void amcNailMarkRange(Seg seg, Addr base, Addr limit)
{
  amcNailboard board;
  Index ibase, ilimit;
  Size headerSize;

  AVER(SegBase(seg) <= base);
  AVER(base < SegLimit(seg));
  AVER(SegBase(seg) <= limit);
  AVER(limit <= SegLimit(seg));
  AVER(base < limit);

  board = amcSegNailboard(seg);
  AVERT(amcNailboard, board);
  headerSize = SegPool(seg)->format->headerSize;
  ibase = (AddrOffset(SegBase(seg), base) + headerSize)
          >> board->markShift;
  ilimit = (AddrOffset(SegBase(seg), limit) + headerSize)
           >> board->markShift;
  AVER(BTIsResRange(board->mark, ibase, ilimit));

  BTSetRange(board->mark, ibase, ilimit);
  board->nails += ilimit - ibase;
  board->distinctNails += ilimit - ibase;
}


/* amcNailRangeIsMarked -- check that a range in the board is marked
 *
 * Like amcNailMarkRange, we take the arguments as referring to base
 * pointers and look at the bits of the corresponding client pointers.
 */
static Bool amcNailRangeIsMarked(Seg seg, Addr base, Addr limit)
{
  amcNailboard board;
  Index ibase, ilimit;
  Size headerSize;

  AVER(SegBase(seg) <= base);
  AVER(base < SegLimit(seg));
  AVER(SegBase(seg) <= limit);
  AVER(limit <= SegLimit(seg));
  AVER(base < limit);

  board = amcSegNailboard(seg);
  AVERT(amcNailboard, board);
  headerSize = SegPool(seg)->format->headerSize;
  ibase = (AddrOffset(SegBase(seg), base) + headerSize)
          >> board->markShift;
  ilimit = (AddrOffset(SegBase(seg), limit) + headerSize)
           >> board->markShift;
  return BTIsSetRange(board->mark, ibase, ilimit);
}


/* amcInitComm -- initialize AMC/Z pool
 *
 * See <design/poolamc/#init>.
 * Shared by AMCInit and AMCZinit.
 */
static Res amcInitComm(Pool pool, RankSet rankSet, va_list arg)
{
  AMC amc;
  Res res;
  Arena arena;
  TraceId ti;
  Trace trace;
  Index i;
  size_t genArraySize;
  size_t genCount;

  AVER(pool != NULL);

  amc = Pool2AMC(pool);
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
  amc->rampMode = RampOUTSIDE;

  TRACE_SET_ITER(ti, trace, TraceSetUNIV, arena)
    STATISTIC(amc->pageretstruct[ti] = pageretstruct_Zero);
  TRACE_SET_ITER_END(ti, trace, TraceSetUNIV, arena);

  if(pool->format->headerSize == 0) {
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

    /* One gen for each one in the chain plus dynamic gen. */
    genArraySize = sizeof(amcGen) * (genCount + 1);
    res = ControlAlloc(&p, arena, genArraySize, FALSE);
    if(res != ResOK)
      goto failGensAlloc;
    amc->gen = p;
    for(i = 0; i < genCount + 1; ++i) {
      res = amcGenCreate(&amc->gen[i], amc, (Serial)i);
      if(res != ResOK) {
        goto failGenAlloc;
      }
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
  EVENT_PP(AMCInit, pool, amc);
  if(rankSet == RankSetEMPTY)
    EVENT_PP(PoolInitAMCZ, pool, pool->format);
  else
    EVENT_PP(PoolInitAMC, pool, pool->format);
  return ResOK;

failGenAlloc:
  while(i > 0) {
    --i;
    amcGenDestroy(amc->gen[i]);
  }
  ControlFree(arena, amc->gen, genArraySize);
failGensAlloc:
  return res;
}

static Res AMCInit(Pool pool, va_list arg)
{
  return amcInitComm(pool, RankSetSingle(RankEXACT), arg);
}

static Res AMCZInit(Pool pool, va_list arg)
{
  return amcInitComm(pool, RankSetEMPTY, arg);
}


/* AMCFinish -- finish AMC pool
 *
 * See <design/poolamc/#finish>.
 */
static void AMCFinish(Pool pool)
{
  AMC amc;
  Ring ring;
  Ring node, nextNode;

  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);

  EVENT_P(AMCFinish, amc);

  /* @@@@ Make sure that segments aren't buffered by forwarding */
  /* buffers.  This is a hack which allows the pool to be destroyed */
  /* while it is collecting.  Note that there aren't any mutator */
  /* buffers by this time. */
  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    BufferDetach(gen->forward, pool);
    /* Maintain invariant < totalSize. */
    gen->pgen.newSize = (Size)0;
  }

  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    Size size;
    amcGen gen = amcSegGen(seg);

    --gen->segs;
    size = SegSize(seg);
    gen->pgen.totalSize -= size;

    SegFree(seg);
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
}


/* AMCBufferFill -- refill an allocation buffer
 *
 * See <design/poolamc/#fill>.
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
  amcGen gen;
  Serial genNr;
  SegPrefStruct segPrefStruct;

  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVERT(Bool, withReservoirPermit);

  gen = amcBufGen(buffer);
  AVERT(amcGen, gen);

  /* Create and attach segment.  The location of this segment is */
  /* expressed as a generation number.  We rely on the arena to */
  /* organize locations appropriately.  */
  arena = PoolArena(pool);
  alignedSize = SizeAlignUp(size, ArenaAlign(arena));
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  genNr = PoolGenNr(&gen->pgen);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &genNr);
  res = SegAlloc(&seg, amcSegClassGet(), &segPrefStruct,
                 alignedSize, pool, withReservoirPermit,
                 &gen->type); /* .segtype */
  if(res != ResOK)
    return res;
  AVER(alignedSize == SegSize(seg));

  /* <design/seg/#field.rankSet.start> */
  if(BufferRankSet(buffer) == RankSetEMPTY)
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  else
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);

  /* Put the segment in the generation indicated by the buffer. */
  ++gen->segs;
  gen->pgen.totalSize += alignedSize;
  /* If ramping, don't count survivors in newSize. */
  if(amc->rampMode != RampRAMPING
     || buffer != amc->rampGen->forward
     || gen != amc->rampGen)
  {
    gen->pgen.newSize += alignedSize;
  } else {
    Seg2amcSeg(seg)->new = FALSE;
  }
  PoolGenUpdateZones(&gen->pgen, seg);

  base = SegBase(seg);
  *baseReturn = base;
  if(alignedSize < AMCLargeSegPAGES * ArenaAlign(arena)) {
    /* Small or Medium segment: give the buffer the entire seg. */
    limit = AddrAdd(base, alignedSize);
    AVER(limit == SegLimit(seg));
  } else {
    /* Large segment: ONLY give the buffer the size requested, and */
    /* pad the remainder of the segment: see job001811. */
    Size padSize;

    limit = AddrAdd(base, size);
    AVER(limit <= SegLimit(seg));
    
    padSize = alignedSize - size;
    AVER(SizeIsAligned(padSize, PoolAlignment(pool)));
    AVER(AddrAdd(limit, padSize) == SegLimit(seg));
    if(padSize > 0) {
      ShieldExpose(arena, seg);
      (*pool->format->pad)(limit, padSize);
      ShieldCover(arena, seg);
    }
  }
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
  AMC amc;
  Size size;
  Arena arena;
  Seg seg;

  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);

  arena = BufferArena(buffer);
  if(SegSize(seg) < AMCLargeSegPAGES * ArenaAlign(arena)) {
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
}


/* AMCRampBegin -- note an entry into a ramp pattern */

static void AMCRampBegin(Pool pool, Buffer buf, Bool collectAll)
{
  AMC amc;

  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
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
  AMC amc;

  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
  AVERT(Buffer, buf);

  AVER(amc->rampCount > 0);
  --amc->rampCount;
  if(amc->rampCount == 0) {
    PoolGen pgen = &amc->rampGen->pgen;
    Ring node, nextNode;

    if(amc->rampMode == RampRAMPING) {
      /* We were ramping, so clean up. */
      amc->rampMode = RampFINISH;
    } else {
      amc->rampMode = RampOUTSIDE;
    }

    /* Adjust amc->rampGen->pgen.newSize: Now count all the segments */
    /* in the ramp generation as new (except if they're white). */
    RING_FOR(node, PoolSegRing(pool), nextNode) {
      Seg seg = SegOfPoolRing(node);

      if(amcSegGen(seg) == amc->rampGen && !Seg2amcSeg(seg)->new
         && SegWhite(seg) == TraceSetEMPTY)
      {
        pgen->newSize += SegSize(seg);
        Seg2amcSeg(seg)->new = TRUE;
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
  amcGen gen;
  AMC amc;
  Buffer buffer;
  Res res;

  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

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
              amcNailMarkRange(seg, BufferScanLimit(buffer),
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
          AVER((BufferScanLimit(buffer) == BufferLimit(buffer))
               || amcNailRangeIsMarked(seg, BufferScanLimit(buffer),
                                       BufferLimit(buffer)));
          /* Nail it for this trace as well. */
          SegSetNailed(seg, TraceSetAdd(SegNailed(seg), trace));
        }
        /* We didn't condemn the buffer, subtract it from the count. */
        /* @@@@ We could subtract all the nailed grains. */
        /* Relies on unsigned arithmetic wrapping round */
        /* on under- and overflow (which it does). */
        trace->condemned -= AddrOffset(BufferScanLimit(buffer),
                                       BufferLimit(buffer));
      }
    }
  }

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  trace->condemned += SegSize(seg);

  amc = Pool2AMC(pool);
  AVERT(AMC, amc);

  STATISTIC_STAT( {
    Count pages;
    AVER(SizeIsAligned(SegSize(seg), ArenaAlign(pool->arena)));
    pages = SegSize(seg) / ArenaAlign(pool->arena);
    AVER(pages != 0);
    amc->pageretstruct[trace->ti].pCond += pages;
    if(pages == 1) {
      amc->pageretstruct[trace->ti].pCS += pages;
    } else if(pages < AMCLargeSegPAGES) {
      amc->pageretstruct[trace->ti].sCM += 1;
      amc->pageretstruct[trace->ti].pCM += pages;
    } else {
      amc->pageretstruct[trace->ti].sCL += 1;
      amc->pageretstruct[trace->ti].pCL += pages;
    }
  } );

  gen = amcSegGen(seg);
  AVERT(amcGen, gen);
  if(Seg2amcSeg(seg)->new) {
    gen->pgen.newSize -= SegSize(seg);
    Seg2amcSeg(seg)->new = FALSE;
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


/* amcScanNailedOnce -- make one scanning pass over a nailed segment
 *
 * *totalReturn set to TRUE iff all objects in segment scanned.
 * *moreReturn set to FALSE only if there are no more objects
 * on the segment that need scanning (which is normally the case).
 * It is set to TRUE if scanning had to be abandoned early on, and
 * also if during emergency fixing any new marks got added to the
 * nailboard.
 */
static Res amcScanNailedOnce(Bool *totalReturn, Bool *moreReturn,
                             ScanState ss, Pool pool, Seg seg, AMC amc)
{
  Addr p, limit;
  Format format;
  Res res;
  Bool total = TRUE;
  Size bytesScanned = 0;

  UNUSED(amc); /* Actually only unused when telemetry is off. @@@@ */

  EVENT_PPP(AMCScanBegin, amc, seg, ss); /* @@@@ use own event */

  format = pool->format;
  amcSegNailboard(seg)->newMarks = FALSE;

  p = AddrAdd(SegBase(seg), format->headerSize);
  while(SegBuffer(seg) != NULL) {
    limit = AddrAdd(BufferScanLimit(SegBuffer(seg)),
                    format->headerSize);
    if(p >= limit) {
      AVER(p == limit);
      goto returnGood;
    }
    while(p < limit) {
      Addr q;
      q = (*format->skip)(p);
      if(amcNailGetMark(seg, p)) {
        res = (*format->scan)(ss, p, q);
        if(res != ResOK) {
          *totalReturn = FALSE;
          *moreReturn = TRUE;
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
    if(amcNailGetMark(seg, p)) {
      res = (*format->scan)(ss, p, q);
      if(res != ResOK) {
        *totalReturn = FALSE;
        *moreReturn = TRUE;
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
  *moreReturn = amcSegNailboard(seg)->newMarks;
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
    res = amcScanNailedOnce(&total, &moreScanning, ss, pool, seg, amc);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    loops += 1;
  } while(moreScanning);

  if(loops > 1) {
    {
      /* Looped: should only happen under emergency tracing. */
      TraceId ti;
      Trace trace;
      Bool emerg = FALSE;
      TraceSet ts = ss->traces;
      Arena arena = pool->arena;
      
      TRACE_SET_ITER(ti, trace, ts, arena)
        if(trace->emergency) {
          emerg = TRUE;
        }
      TRACE_SET_ITER_END(ti, trace, ts, arena);
      AVER(emerg);
    }
    {      
      /* Looped: fixed refs (from 1st pass) were seen by MPS_FIX1
       * (in later passes), so the "ss.unfixedSummary" is _not_ 
       * purely unfixed.  In this one case, unfixedSummary is not 
       * accurate, and cannot be used to verify the SegSummary (see 
       * impl/trace/#verify.segsummary).  Use ScanStateSetSummary to 
       * store ScanStateSummary in ss.fixedSummary and reset 
       * ss.unfixedSummary.  See job001548.
       */
      RefSet refset;
    
      refset = ScanStateSummary(ss);

#if 1
      /* A rare event, which might prompt a rare defect to appear. */
      DIAG_SINGLEF(( "amcScanNailed_loop",
        "scan completed, but had to loop $U times:\n", (WriteFU)loops,
        " SegSummary:        $B\n", (WriteFB)SegSummary(seg),
        " ss.white:          $B\n", (WriteFB)ss->white,
        " ss.unfixedSummary: $B", (WriteFB)ss->unfixedSummary,
          "$S\n", (WriteFS)( 
            (RefSetSub(ss->unfixedSummary, SegSummary(seg)))
            ? ""
            : " <=== This would have failed .verify.segsummary!"
            ),
        " ss.fixedSummary:   $B\n", (WriteFB)ss->fixedSummary,
        "ScanStateSummary:   $B\n", (WriteFB)refset,
        "MOVING ScanStateSummary TO fixedSummary, "
        "RESETTING unfixedSummary.\n", NULL
      ));
#endif
    
      ScanStateSetSummary(ss, refset);
    }
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
  Arena arena;
  Format format;
  AMC amc;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVERT(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);


  format = pool->format;
  arena = pool->arena;

  if(amcSegHasNailboard(seg)) {
    return amcScanNailed(totalReturn, ss, pool, seg, amc);
  }

  EVENT_PPP(AMCScanBegin, amc, seg, ss);

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
    res = (*format->scan)(ss, base, limit);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    ss->scannedSize += AddrOffset(base, limit);
    base = limit;
  }

  /* <design/poolamc/#seg-scan.finish> @@@@ base? */
  limit = AddrAdd(SegLimit(seg), format->headerSize);
  AVER(SegBase(seg) <= base);
  AVER(base <= AddrAdd(SegLimit(seg), format->headerSize));
  if(base < limit) {
    res = (*format->scan)(ss, base, limit);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  }

  ss->scannedSize += AddrOffset(base, limit);
  EVENT_PPP(AMCScanEnd, amc, seg, ss);

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

  EVENT_0(AMCFixInPlace);
  if(amcSegHasNailboard(seg)) {
    Bool wasMarked = amcNailGetAndSetMark(seg, ref);
    /* If there are no new marks (i.e., no new traces for which we */
    /* are marking, and no new mark bits set) then we can return */
    /* immediately, without changing colour. */
    if(TraceSetSub(ss->traces, SegNailed(seg)) && wasMarked)
      return;
  } else if(TraceSetSub(ss->traces, SegNailed(seg))) {
    return;
  }
  SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
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
  AMC amc;
  Addr newRef;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  amc = Pool2AMC(pool);
  AVERT(AMC, amc);

  ss->wasMarked = TRUE;

  if(ss->rank == RankAMBIG)
    goto fixInPlace;

  ShieldExpose(arena, seg);
  newRef = (*pool->format->isMoved)(*refIO);
  ShieldCover(arena, seg);
  if(newRef != (Addr)0) {
    /* Object has been forwarded already, so snap-out pointer. */
    /* Useful weak pointer semantics not implemented. @@@@ */
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
Res AMCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Res res;
  Format format;       /* cache of pool->format */
  Ref ref;             /* reference to be fixed */
  Ref newRef;          /* new location, if moved */
  Size length;         /* length of object to be relocated */
  Buffer buffer;       /* buffer to allocate new copy into */
  amcGen gen;          /* generation of old copy of object */
  TraceSet grey;       /* greyness of object being relocated */
  TraceSet toGrey;     /* greyness of object's destination */
  RefSet summary;      /* summary of object being relocated */
  RefSet toSummary;    /* summary of object's destination */
  Seg toSeg;           /* segment to which object is being relocated */

  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);
  EVENT_0(AMCFix);

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

  amc = Pool2AMC(pool);
  AVERT_CRITICAL(AMC, amc);
  format = pool->format;
  ref = *refIO;
  AVER_CRITICAL(SegBase(seg) <= ref);
  AVER_CRITICAL(ref < SegLimit(seg));
  arena = pool->arena;

  /* .exposed.seg: Statements tagged ".exposed.seg" below require */
  /* that "seg" (that is: the 'from' seg) has been ShieldExposed. */
  ShieldExpose(arena, seg);
  newRef = (*format->isMoved)(ref);  /* .exposed.seg */

  if(newRef == (Addr)0) {
    /* If object is nailed already then we mustn't copy it: */
    if(SegNailed(seg) != TraceSetEMPTY
       && (!amcSegHasNailboard(seg) || amcNailGetMark(seg, ref))) {
      /* Segment only needs greying if there are new traces for */
      /* which we are nailing. */
      if(!TraceSetSub(ss->traces, SegNailed(seg))) {
        if(SegRankSet(seg) != RankSetEMPTY) {
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
        }
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
    EVENT_A(AMCFixForward, newRef);
    /* <design/fix/#protocol.was-marked> */
    ss->wasMarked = FALSE;

    /* Get the forwarding buffer from the object's generation. */
    gen = amcSegGen(seg);
    buffer = gen->forward;
    AVER_CRITICAL(buffer != NULL);

    length = AddrOffset(ref, (*format->skip)(ref));  /* .exposed.seg */
    STATISTIC_STAT(++ss->forwardedCount);
    ss->forwardedSize += length;
    do {
      res = BUFFER_RESERVE(&newRef, buffer, length, FALSE);
      if(res != ResOK)
        goto returnRes;

      toSeg = BufferSeg(buffer);
      ShieldExpose(arena, toSeg);

      /* Since we're moving an object from one segment to another, */
      /* union the greyness and the summaries together. */
      grey = TraceSetUnion(ss->traces, SegGrey(seg));
      toGrey = SegGrey(toSeg);
      if(TraceSetDiff(grey, toGrey) != TraceSetEMPTY
          && SegRankSet(seg) != RankSetEMPTY) {
        SegSetGrey(toSeg, TraceSetUnion(toGrey, grey));
      }
      summary = SegSummary(seg);
      toSummary = SegSummary(toSeg);
      if(RefSetDiff(summary, toSummary) != RefSetEMPTY) {
        SegSetSummary(toSeg, RefSetUnion(toSummary, summary));
      }

      /* <design/trace/#fix.copy> */
      (void)AddrCopy(newRef, ref, length);  /* .exposed.seg */

      ShieldCover(arena, toSeg);
    } while(!BUFFER_COMMIT(buffer, newRef, length));
    ss->copiedSize += length;

    (*format->move)(ref, newRef);  /* .exposed.seg */
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
  ShieldCover(arena, seg);  /* .exposed.seg */
  return res;
}


/* AMCHeaderFix -- fix a reference to the pool, with headers
 *
 * See <design/poolamc/#header.fix>.
 */
static Res AMCHeaderFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Arena arena;
  AMC amc;
  Res res;
  Format format;       /* cache of pool->format */
  Ref ref;             /* reference to be fixed */
  Ref newRef;          /* new location, if moved */
  Addr newBase;        /* base address of new copy */
  Size length;         /* length of object to be relocated */
  Buffer buffer;       /* buffer to allocate new copy into */
  amcGen gen;          /* generation of old copy of object */
  TraceSet grey;       /* greyness of object being relocated */
  TraceSet toGrey;     /* greyness of object's destination */
  RefSet summary;      /* summary of object being relocated */
  RefSet toSummary;    /* summary of object's destination */
  Seg toSeg;           /* segment to which object is being relocated */

  /* <design/trace/#fix.noaver> */
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);
  EVENT_0(AMCFix);

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

  amc = Pool2AMC(pool);
  AVERT_CRITICAL(AMC, amc);
  format = pool->format;
  ref = *refIO;
  AVER_CRITICAL(AddrAdd(SegBase(seg), pool->format->headerSize)
                <= ref);
  AVER_CRITICAL(ref < SegLimit(seg)); /* see .ref-limit */
  arena = pool->arena;

  /* .exposed.seg: Statements tagged ".exposed.seg" below require */
  /* that "seg" (that is: the 'from' seg) has been ShieldExposed. */
  ShieldExpose(arena, seg);
  newRef = (*format->isMoved)(ref);  /* .exposed.seg */

  if(newRef == (Addr)0) {
    /* If object is nailed already then we mustn't copy it: */
    if(SegNailed(seg) != TraceSetEMPTY
       && (!amcSegHasNailboard(seg) || amcNailGetMark(seg, ref))) {
      /* Segment only needs greying if there are new traces for */
      /* which we are nailing. */
      if(!TraceSetSub(ss->traces, SegNailed(seg))) {
        if(SegRankSet(seg) != RankSetEMPTY)
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
        SegSetNailed(seg, TraceSetUnion(SegNailed(seg), ss->traces));
      }
      res = ResOK;
      goto returnRes;
    } else if(ss->rank == RankWEAK) {
      /* object is not preserved (neither moved, nor nailed) */
      /* hence, reference should be splatted */
      goto updateReference;
    }
    /* object is not preserved yet (neither moved, nor nailed) */
    /* so should be preserved by forwarding */
    EVENT_A(AMCFixForward, newRef);
    /* <design/fix/#protocol.was-marked> */
    ss->wasMarked = FALSE;

    /* Get the forwarding buffer from the object's generation. */
    gen = amcSegGen(seg);
    buffer = gen->forward;
    AVER_CRITICAL(buffer != NULL);

    length = AddrOffset(ref, (*format->skip)(ref));  /* .exposed.seg */
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
      if(TraceSetDiff(grey, toGrey) != TraceSetEMPTY
          && SegRankSet(seg) != RankSetEMPTY)
        SegSetGrey(toSeg, TraceSetUnion(toGrey, grey));
      summary = SegSummary(seg);
      toSummary = SegSummary(toSeg);
      if(RefSetDiff(summary, toSummary) != RefSetEMPTY)
        SegSetSummary(toSeg, RefSetUnion(toSummary, summary));

      /* <design/trace/#fix.copy> */
      (void)AddrCopy(newBase, AddrSub(ref, headerSize), length);  /* .exposed.seg */

      ShieldCover(arena, toSeg);
    } while (!BUFFER_COMMIT(buffer, newBase, length));
    ss->copiedSize += length;

    (*format->move)(ref, newRef);  /* .exposed.seg */
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
  AMC amc;
  Size headerSize;
  Addr p1;  /* first obj in seg */
  Bool obj1pip = FALSE;  /* first obj was preserved in place */

  /* All arguments AVERed by AMCReclaim */

  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
  format = pool->format;

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* see <design/poolamc/#nailboard.limitations> for improvements */
  headerSize = format->headerSize;
  ShieldExpose(arena, seg);
  p = AddrAdd(SegBase(seg), headerSize);
  if(SegBuffer(seg) != NULL) {
    limit = BufferScanLimit(SegBuffer(seg));
  } else {
    limit = SegLimit(seg);
  }
  limit = AddrAdd(limit, headerSize);
  p1 = p;
  while(p < limit) {
    Addr q;
    Size length;
    q = (*format->skip)(p);
    length = AddrOffset(p, q);
    if(amcSegHasNailboard(seg)
        ? !amcNailGetMark(seg, p)
        /* If there's no mark table, retain all that hasn't been */
        /* forwarded.  In this case, preservedInPlace* become */
        /* somewhat overstated. */
        : (*format->isMoved)(p) != NULL)
    {
      /* Replace forwarding pointer / unreachable object with pad */
      (*format->pad)(AddrSub(p, headerSize), length);
      bytesReclaimed += length;
    } else {
      ++preservedInPlaceCount;
      preservedInPlaceSize += length;
      if(p == p1)
        obj1pip = TRUE;
    }
    
    AVER(p < q);
    p = q;
  }
  AVER(p == limit);
  ShieldCover(arena, seg);

  SegSetNailed(seg, TraceSetDel(SegNailed(seg), trace));
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  if(SegNailed(seg) == TraceSetEMPTY && amcSegHasNailboard(seg)) {
    amcSegDestroyNailboard(seg, pool);
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

    --gen->segs;
    gen->pgen.totalSize -= SegSize(seg);
    SegFree(seg);
  } else {
    /* Seg retained */
    STATISTIC_STAT( {
      Count pages;
      AVER(SizeIsAligned(SegSize(seg), ArenaAlign(pool->arena)));
      pages = SegSize(seg) / ArenaAlign(pool->arena);
      AVER(pages != 0);
      amc->pageretstruct[trace->ti].pRet += pages;
      if(pages == 1) {
        amc->pageretstruct[trace->ti].pRS += pages;
      } else if(pages < AMCLargeSegPAGES) {
        amc->pageretstruct[trace->ti].sRM += 1;
        amc->pageretstruct[trace->ti].pRM += pages;
        if(obj1pip) {
          amc->pageretstruct[trace->ti].pRM1 += pages;
        } else {
          /* Seg retained by a rest obj.  Cost: one rest page, */
          /* plus pages-1 pages of pure padding. */
          amc->pageretstruct[trace->ti].pRMrr += 1;
          amc->pageretstruct[trace->ti].pRMr1 += pages - 1;
        }
      } else {
        amc->pageretstruct[trace->ti].sRL += 1;
        amc->pageretstruct[trace->ti].pRL += pages;
        if(!obj1pip) {
          /* Seg retained by a rest obj */
          amc->pageretstruct[trace->ti].pRLr += pages;
        }
      }
    } );

  }
}


/* AMCReclaim -- recycle a segment if it is still white
 *
 * See <design/poolamc/#reclaim>.
 */
static void AMCReclaim(Pool pool, Trace trace, Seg seg)
{
  AMC amc;
  amcGen gen;
  Size size;

  AVERT_CRITICAL(Pool, pool);
  amc = Pool2AMC(pool);
  AVERT_CRITICAL(AMC, amc);
  AVERT_CRITICAL(Trace, trace);
  AVERT_CRITICAL(Seg, seg);

  gen = amcSegGen(seg);
  AVERT_CRITICAL(amcGen, gen);

  EVENT_PPP(AMCReclaim, gen, trace, seg);

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

  --gen->segs;
  size = SegSize(seg);
  gen->pgen.totalSize -= size;

  trace->reclaimSize += size;

  SegFree(seg);
}


/* AMCTraceEnd -- emit end-of-trace diagnostics
 *
 */
static void AMCTraceEnd(Pool pool, Trace trace)
{
  AMC amc;
  TraceId ti;
  Count pRetMin = 100;
  
  AVERT(Pool, pool);
  AVERT(Trace, trace);

  amc = Pool2AMC(pool);
  AVERT(AMC, amc);
  ti = trace->ti;
  AVER(TraceIdCheck(ti));

  if(amc->pageretstruct[ti].pRet >= pRetMin) {
    DIAG_SINGLEF(( "AMCTraceEnd_pageret",
      " $U", (WriteFU)ArenaEpoch(pool->arena),
      " $U", (WriteFU)trace->why,
      " $S", (WriteFS)(trace->emergency ? "Emergency" : "-"),
      " $U", (WriteFU)amc->pageretstruct[ti].pCond,
      " $U", (WriteFU)amc->pageretstruct[ti].pRet, ",",
      " $U", (WriteFU)amc->pageretstruct[ti].pCS,
      " $U", (WriteFU)amc->pageretstruct[ti].pRS, ",",
      " $U", (WriteFU)amc->pageretstruct[ti].sCM,
      " $U", (WriteFU)amc->pageretstruct[ti].pCM,
      " $U", (WriteFU)amc->pageretstruct[ti].sRM,
      " $U", (WriteFU)amc->pageretstruct[ti].pRM,
      " $U", (WriteFU)amc->pageretstruct[ti].pRM1,
      " $U", (WriteFU)amc->pageretstruct[ti].pRMrr,
      " $U", (WriteFU)amc->pageretstruct[ti].pRMr1, ",",
      " $U", (WriteFU)amc->pageretstruct[ti].sCL,
      " $U", (WriteFU)amc->pageretstruct[ti].pCL,
      " $U", (WriteFU)amc->pageretstruct[ti].sRL,
      " $U", (WriteFU)amc->pageretstruct[ti].pRL,
      " $U", (WriteFU)amc->pageretstruct[ti].pRLr,
      " (page = $Ub,", (WriteFU)ArenaAlign(pool->arena), 
      " Large >= $Up,", (WriteFU)AMCLargeSegPAGES, 
      " pRetMin $U)", (WriteFU)pRetMin,
      NULL ));
  }

  STATISTIC(amc->pageretstruct[ti] = pageretstruct_Zero);
}


/* AMCWalk -- Apply function to (black) objects in segment */

static void AMCWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                    void *p, size_t s)
{
  Addr object, nextObject, limit;
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
  /* are not handled properly:  No objects are walked.  See */
  /* job001682. */
  if(SegWhite(seg) == TraceSetEMPTY && SegGrey(seg) == TraceSetEMPTY
     && SegNailed(seg) == TraceSetEMPTY)
  {
    amc = Pool2AMC(pool);
    AVERT(AMC, amc);
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

static void amcWalkAll(Pool pool, FormattedObjectsStepMethod f,
                       void *p, size_t s)
{
  Arena arena;
  Ring ring, next, node;

  AVER(IsSubclassPoly(pool->class, AMCPoolClassGet()));

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
 * See <design/poolamc/#describe>.
 */
static Res AMCDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  AMC amc;
  Ring node, nextNode;
  char *rampmode;

  if(!CHECKT(Pool, pool))
    return ResFAIL;
  amc = Pool2AMC(pool);
  if(!CHECKT(AMC, amc))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               (amc->rankSet == RankSetEMPTY) ? "AMCZ" : "AMC",
               " $P {\n", (WriteFP)amc, "  pool $P ($U)\n",
               (WriteFP)AMC2Pool(amc), (WriteFU)AMC2Pool(amc)->serial,
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
  res = WriteF(stream,
               "  ", rampmode, " ($U)\n", (WriteFU)amc->rampCount,
               NULL);
  if(res != ResOK)
    return res;

  RING_FOR(node, &amc->genRing, nextNode) {
    amcGen gen = RING_ELT(amcGen, amcRing, node);
    res = amcGenDescribe(gen, stream);
    if(res != ResOK)
      return res;
  }

  if(0) {
    /* SegDescribes */
    RING_FOR(node, &AMC2Pool(amc)->segRing, nextNode) {
      Seg seg = RING_ELT(Seg, poolRing, node);
      res = AMCSegDescribe(seg, stream);
      if(res != ResOK)
        return res;
    }
  }

  res = WriteF(stream, "} AMC $P\n", (WriteFP)amc, NULL);
  if(res != ResOK)
    return res;

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
  this->traceEnd = AMCTraceEnd;
  this->rampBegin = AMCRampBegin;
  this->rampEnd = AMCRampEnd;
  this->walk = AMCWalk;
  this->bufferClass = amcBufClassGet;
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
  return (mps_class_t)AMCPoolClassGet();
}

/* mps_class_amcz -- return the pool class descriptor to the client */

mps_class_t mps_class_amcz(void)
{
  return (mps_class_t)AMCZPoolClassGet();
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
  void (*f)(mps_addr_t object, void *p, size_t s);
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
static Bool AMCCheck(AMC amc)
{
  CHECKS(AMC, amc);
  CHECKD(Pool, &amc->poolStruct);
  CHECKL(IsSubclassPoly(amc->poolStruct.class, EnsureAMCPoolClass()));
  CHECKL(RankSetCheck(amc->rankSet));
  CHECKL(RingCheck(&amc->genRing));
  CHECKL(BoolCheck(amc->gensBooted));
  if(amc->gensBooted) {
    CHECKD(amcGen, amc->nursery);
    CHECKL(amc->gen != NULL);
    CHECKD(amcGen, amc->rampGen);
    CHECKD(amcGen, amc->afterRampGen);
  }
  /* nothing to check for rampCount */
  CHECKL(amc->rampMode >= RampOUTSIDE);
  CHECKL(amc->rampMode <= RampCOLLECTING);
  /* pageretstruct[ti] is statistics only, currently unchecked */

  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

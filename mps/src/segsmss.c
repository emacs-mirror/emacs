/* impl.c.segsmss: Segment splitting and merging stress test
 *
 * $HopeName: MMsrc!segsmss.c(trunk.4) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .design: Adapted from amsss.c (because AMS already supports 
 * a protocol for subclassing AMS segments). Defines a new pool 
 * class, AMST. Segments are split and merged during BufferFill
 * operations. Buffered segments are also split and merged between
 * allocation requests.
 */

#include "mpm.h"
#include "poolams.h"
#include "fmtdy.h"
#include "testlib.h"
#include "mpscams.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>


/* Forward declarations */

extern SegClass EnsureAMSTSegClass(void);
extern PoolClass EnsureAMSTPoolClass(void);


/* Start by defining the AMST pool (AMS Test pool) */

#define AMSTSig         ((Sig)0x519A3529) /* SIGnature AMST */

/* AMSTStruct -- AMST pool instance structure */

typedef struct AMSTStruct {
  AMSStruct amsStruct;      /* generic AMS structure */
  Bool failSegs;            /* fail seg splits & merges when true */
  Count splits;             /* count of successful segment splits */
  Count merges;             /* count of successful segment merges */
  Count badSplits;          /* count of unsuccessful segment splits */
  Count badMerges;          /* count of unsuccessful segment merges */
  Count bsplits;            /* count of buffered segment splits */
  Count bmerges;            /* count of buffered segment merges */
  Sig sig;                  /* design.mps.pool.outer-structure.sig */
} AMSTStruct;

typedef struct AMSTStruct *AMST;

#define PoolPoolAMST(pool) PARENT(AMSTStruct, amsStruct.poolStruct, (pool))
#define AMSTPoolAMS(amst)  (&(amst)->amsStruct)


/* AMSTCheck -- the check method for an AMST */

static Bool AMSTCheck(AMST amst)
{
  CHECKS(AMST, amst);
  CHECKL(AMSCheck(AMSTPoolAMS(amst)));
  return TRUE;
}

/* AMSTFailOperation -- should a split/merge operation fail?
 *
 * returns TRUE if so.
 */

static Bool AMSTFailOperation(AMST amst)
{
  if(amst->failSegs) {
    return rnd() % 2;
  } else {
    return FALSE;
  }
}

/* AMSTSegStruct: AMST segment instances */

#define AMSTSegSig     ((Sig)0x519A3525) /* SIGnature AMST Seg */

typedef struct AMSTSegStruct *AMSTSeg;

typedef struct AMSTSegStruct {
  AMSSegStruct amsSegStruct; /* superclass fields must come first */
  AMSTSeg next;          /* mergeable next segment, or NULL */
  AMSTSeg prev;          /* mergeable prev segment, or NULL */
  Sig sig;               /* design.mps.pool.outer-structure.sig */
} AMSTSegStruct;



/* AMSTSegCheck -- check the AMST segment */

static Bool AMSTSegCheck(AMSTSeg amstseg)
{
  CHECKS(AMSTSeg, amstseg);
  CHECKL(AMSSegCheck(&amstseg->amsSegStruct));
  /* don't bother to do other checks - this is a stress test */
  return TRUE;
}

#define SegAMSTSeg(seg)             ((AMSTSeg)(seg))
#define AMSTSegSeg(amstseg)         ((Seg)(amstseg))


/* amstSegInit -- initialise an amst segment */

static Res amstSegInit(Seg seg, Pool pool, Addr base, Size size, 
                       Bool reservoirPermit, va_list args)
{
  SegClass super;
  AMSTSeg amstseg;
  AMST amst;
  Res res;

  AVERT(Seg, seg);
  amstseg = SegAMSTSeg(seg);
  AVERT(Pool, pool);
  amst = PoolPoolAMST(pool);
  AVERT(AMST, amst);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(AMSTSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if(res != ResOK)
    return res;

  amstseg->next = NULL;
  amstseg->prev = NULL;
  amstseg->sig = AMSTSegSig;
  AVERT(AMSTSeg, amstseg);

  return ResOK;
}


/* amstSegFinish -- Finish method for AMST segments */

static void amstSegFinish(Seg seg)
{
  SegClass super;
  AMSTSeg amstseg;

  AVERT(Seg, seg);
  amstseg = SegAMSTSeg(seg);
  AVERT(AMSTSeg, amstseg);

  if(amstseg->next != NULL)
    amstseg->next->prev = NULL;
  if(amstseg->prev != NULL)
    amstseg->prev->next = NULL;

  amstseg->sig = SigInvalid;
  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(AMSTSegClass);
  super->finish(seg);
}  



/* amstSegMerge -- AMSTSeg merge method 
 *
 * .fail: Test proper handling of the most complex failure cases 
 * by deliberately detecting failure sometimes after calling the 
 * next method. We handle the error by calling the anti-method. 
 * This isn't strictly safe (see design.mps.poolams.split-merge.fail).
 * But we assume here that we won't run out of memory when calling the 
 * anti-method.
 */

static Res amstSegMerge(Seg seg, Seg segHi, 
                        Addr base, Addr mid, Addr limit,
                        Bool withReservoirPermit, va_list args)
{
  SegClass super;
  AMST amst;
  AMSTSeg amstseg, amstsegHi;
  Res res;

  AVERT(Seg, seg);
  AVERT(Seg, segHi);
  amstseg = SegAMSTSeg(seg);
  amstsegHi = SegAMSTSeg(segHi);
  AVERT(AMSTSeg, amstseg);
  AVERT(AMSTSeg, amstsegHi);
  amst = PoolPoolAMST(SegPool(seg));

  /* Merge the superclass fields via direct next-method call */
  super = SEG_SUPERCLASS(AMSTSegClass);
  res = super->merge(seg, segHi, base, mid, limit, 
                     withReservoirPermit, args);
  if(res != ResOK)
    goto failSuper;

  if(AMSTFailOperation(amst)) {
    amst->badMerges++;
    printf("D");
    goto failDeliberate;
  }

  amstseg->next = amstsegHi->next;
  amstsegHi->sig = SigInvalid;
  AVERT(AMSTSeg, amstseg);
  amst->merges++;
  printf("M");
  return ResOK;

failDeliberate:
  /* Call the anti-method (see .fail) */
  res = super->split(seg, segHi, base, mid, limit, 
                     withReservoirPermit, args);
  AVER(res == ResOK);
  res = ResFAIL;
failSuper:
  AVERT(AMSTSeg, amstseg);
  AVERT(AMSTSeg, amstsegHi);
  return res;
}


/* amstSegSplit -- AMSTSeg split method */

static Res amstSegSplit(Seg seg, Seg segHi, 
                        Addr base, Addr mid, Addr limit,
                        Bool withReservoirPermit, va_list args)
{
  SegClass super;
  AMST amst;
  AMSTSeg amstseg, amstsegHi;
  Res res;

  AVERT(Seg, seg);
  AVER(segHi != NULL);  /* can't check fully, it's not initialized */
  amstseg = SegAMSTSeg(seg);
  amstsegHi = SegAMSTSeg(segHi);
  AVERT(AMSTSeg, amstseg);
  amst = PoolPoolAMST(SegPool(seg));

  /* Split the superclass fields via direct next-method call */
  super = SEG_SUPERCLASS(AMSTSegClass);
  res = super->split(seg, segHi, base, mid, limit, 
                     withReservoirPermit, args);
  if(res != ResOK)
    goto failSuper;

  if(AMSTFailOperation(amst)) {
    amst->badSplits++;
    printf("B");
    goto failDeliberate;
  }

  /* Full initialization for segHi. */
  amstsegHi->next = amstseg->next;
  amstsegHi->prev = amstseg;
  amstsegHi->sig = AMSTSegSig;
  amstseg->next = amstsegHi;
  AVERT(AMSTSeg, amstseg);
  AVERT(AMSTSeg, amstsegHi);
  amst->splits++;
  printf("S");
  return ResOK;

failDeliberate:
  /* Call the anti-method. (see .fail) */
  res = super->merge(seg, segHi, base, mid, limit, 
                     withReservoirPermit, args);
  AVER(res == ResOK);
  res = ResFAIL;
failSuper:
  AVERT(AMSTSeg, amstseg);
  return res;
}


/* AMSTSegClass -- Class definition for AMST segments */

DEFINE_SEG_CLASS(AMSTSegClass, class)
{
  INHERIT_CLASS(class, AMSSegClass);
  class->name = "AMSTSEG";
  class->size = sizeof(AMSTSegStruct);
  class->init = amstSegInit;
  class->finish = amstSegFinish;
  class->split = amstSegSplit;
  class->merge = amstSegMerge;
}


/* AMSTSegSizePolicy
 *
 * Picks double the default segment size.
 */

static Res AMSTSegSizePolicy(Size *sizeReturn,
                             Pool pool, Size size, RankSet rankSet)
{
  Arena arena;
  Size basic, want;

  AVER(sizeReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(RankSetCheck(rankSet));

  arena = PoolArena(pool);

  basic = SizeAlignUp(size, ArenaAlign(arena));
  if(basic == 0) {
    /* overflow */
    return ResMEMORY;
  }
  want = basic + basic;
  if(want <= basic) {
    /* overflow */
    return ResMEMORY;
  }
  *sizeReturn = want;
  return ResOK;
}


/* AMSTInit -- the pool class initialization method */

static Res AMSTInit(Pool pool, va_list args)
{
  AMST amst; AMS ams;
  Format format;
  Res res;

  AVERT(Pool, pool);

  format = va_arg(args, Format);
  res = AMSInitInternal(PoolPoolAMS(pool), format);
  if(res != ResOK)
    return res;
  amst = PoolPoolAMST(pool);
  ams = PoolPoolAMS(pool);
  ams->segSize = AMSTSegSizePolicy;
  ams->segClass = EnsureAMSTSegClass;
  amst->failSegs = TRUE;
  amst->splits = 0;
  amst->merges = 0;
  amst->badSplits = 0;
  amst->badMerges = 0;
  amst->bsplits = 0;
  amst->bmerges = 0;
  amst->sig = AMSTSig;
  AVERT(AMST, amst);
  return ResOK;
}


/* AMSTFinish -- the pool class finish method */

static void AMSTFinish(Pool pool)
{
  AMST amst;

  AVERT(Pool, pool);
  amst = PoolPoolAMST(pool);
  AVERT(AMST, amst);

  printf("\nDestroying pool, having performed:\n");
  printf("    %lu splits          (S)\n", (unsigned long)amst->splits);
  printf("    %lu merges          (M)\n", (unsigned long)amst->merges);
  printf("    %lu aborted splits  (B)\n", (unsigned long)amst->badSplits);
  printf("    %lu aborted merges  (D)\n", (unsigned long)amst->badMerges);
  printf("  which included:\n");
  printf("    %lu buffered splits (C)\n", (unsigned long)amst->bsplits);
  printf("    %lu buffered merges (J)\n", (unsigned long)amst->bmerges);

  AMSFinish(pool);
  amst->sig = SigInvalid;
}


/* AMSSegIsFree -- return TRUE if a seg is all unallocated */

static Bool AMSSegIsFree(Seg seg)
{
  AMSSeg amsseg;
  AVERT(Seg, seg);
  amsseg = SegAMSSeg(seg);
  return(amsseg->free == amsseg->grains);
}


/* AMSSegRegionIsFree -- return TRUE if a region is all unallocated */

static Bool AMSSegRegionIsFree(Seg seg, Addr base, Addr limit)
{
  AMSSeg amsseg;
  AMS ams;
  Count bgrain, lgrain;
  Addr sbase;

  AVERT(Seg, seg);
  amsseg = SegAMSSeg(seg);
  sbase = SegBase(seg);
  ams = PoolPoolAMS(SegPool(seg));

  bgrain = AMSGrains(ams, AddrOffset(sbase, base)); 
  lgrain = AMSGrains(ams, AddrOffset(sbase, limit));

  if(amsseg->allocTableInUse) {
    return BTIsResRange(amsseg->allocTable, bgrain, lgrain);
  } else {
    return amsseg->firstFree <= bgrain;
  }
}


/* AMSUnallocateRange -- set a range to be unallocated
 *
 * Used as a means of overriding the behaviour of AMSBufferFill.
 * The code is similar to AMSBufferEmpty.
 */

static void AMSUnallocateRange(Seg seg, Addr base, Addr limit)
{
  Pool pool;
  AMS ams;
  AMSSeg amsseg;
  Index baseIndex, limitIndex;
  /* parameters checked by caller */

  pool = SegPool(seg);
  ams = PoolPoolAMS(pool);
  amsseg = SegAMSSeg(seg);

  baseIndex = AMS_ADDR_INDEX(seg, base);
  limitIndex = AMS_ADDR_INDEX(seg, limit);

  if(amsseg->allocTableInUse) {
    /* check that it's allocated */
    AVER(BTIsSetRange(amsseg->allocTable, baseIndex, limitIndex));
    BTResRange(amsseg->allocTable, baseIndex, limitIndex);
  } else {
    /* check that it's allocated */
    AVER(limitIndex <= amsseg->firstFree);
    if(limitIndex == amsseg->firstFree) /* is it at the end? */ {
      amsseg->firstFree = baseIndex;
    } else { /* start using allocTable */
      amsseg->allocTableInUse = TRUE;
      BTSetRange(amsseg->allocTable, 0, amsseg->firstFree);
      if(amsseg->firstFree < amsseg->grains)
        BTResRange(amsseg->allocTable, amsseg->firstFree, amsseg->grains);
      BTResRange(amsseg->allocTable, baseIndex, limitIndex);
    }
  }
  amsseg->free += limitIndex - baseIndex;
}


/* AMSAllocateRange -- set a range to be allocated
 *
 * Used as a means of overriding the behaviour of AMSBufferFill.
 * The code is similar to AMSUnallocateRange.
 */

static void AMSAllocateRange(Seg seg, Addr base, Addr limit)
{
  Pool pool;
  AMS ams;
  AMSSeg amsseg;
  Index baseIndex, limitIndex;
  /* parameters checked by caller */

  pool = SegPool(seg);
  ams = PoolPoolAMS(pool);
  amsseg = SegAMSSeg(seg);

  baseIndex = AMS_ADDR_INDEX(seg, base);
  limitIndex = AMS_ADDR_INDEX(seg, limit);

  if(amsseg->allocTableInUse) {
    /* check that it's not allocated */
    AVER(BTIsResRange(amsseg->allocTable, baseIndex, limitIndex));
    BTSetRange(amsseg->allocTable, baseIndex, limitIndex);
  } else {
    /* check that it's not allocated */
    AVER(baseIndex >= amsseg->firstFree);
    if(baseIndex == amsseg->firstFree) /* is it at the end? */ {
      amsseg->firstFree = limitIndex;
    } else { /* start using allocTable */
      amsseg->allocTableInUse = TRUE;
      BTSetRange(amsseg->allocTable, 0, amsseg->firstFree);
      if(amsseg->firstFree < amsseg->grains)
        BTResRange(amsseg->allocTable, amsseg->firstFree, amsseg->grains);
      BTSetRange(amsseg->allocTable, baseIndex, limitIndex);
    }
  }
  AVER(amsseg->free >= limitIndex - baseIndex);
  amsseg->free -= limitIndex - baseIndex;
}


/* AMSTBufferFill -- the pool class buffer fill method
 * 
 * Calls next method - but possibly splits or merges the chosen 
 * segment.
 *
 * .merge: A merge is performed when the next method returns 
 * the entire segment, this segment had previously been split
 * from the segment below, and the segment below is appropriately
 * similar (i.e. not already attached to a buffer and similarly grey)
 *
 * .split: If we're not merging, a split is performed if the next method 
 * returns the entire segment, and yet lower half of the segment would 
 * meet the request.
 */

static Res AMSTBufferFill(Addr *baseReturn, Addr *limitReturn,
                   Pool pool, Buffer buffer, Size size,
                   Bool withReservoirPermit)
{
  PoolClass super;
  Addr base, limit;
  Arena arena;
  AMST amst;
  Bool b;
  Seg seg;
  AMSTSeg amstseg;
  Res res;

  AVERT(Pool, pool);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  /* other parameters are checked by next method */
  arena = PoolArena(pool);
  amst = PoolPoolAMST(pool);

  /* call next method */
  super = POOL_SUPERCLASS(AMSTPoolClass);
  res = super->bufferFill(&base, &limit, pool, buffer, size,
                          withReservoirPermit);
  if(res != ResOK)
    return res;

  b = SegOfAddr(&seg, arena, base);
  AVER(b);
  amstseg = SegAMSTSeg(seg);

  if(SegLimit(seg) == limit && SegBase(seg) == base) {
    if(amstseg->prev != NULL) {
      Seg segLo = AMSTSegSeg(amstseg->prev);
      if(SegBuffer(segLo) == NULL && SegGrey(segLo) == SegGrey(seg)) {
        /* .merge */
        Seg mergedSeg;
        Res mres;
        AMSUnallocateRange(seg, base, limit);
        mres = SegMerge(&mergedSeg, segLo, seg, withReservoirPermit);
        if(ResOK == mres) { /* successful merge */
          AMSAllocateRange(mergedSeg, base, limit);
          /* leave range as-is */
        } else {            /* failed to merge */
          AVER(amst->failSegs); /* deliberate fails only */
          AMSAllocateRange(seg, base, limit);
        }
      }

    } else {
      Size half = SegSize(seg) / 2;
      if (half >= size && SizeIsAligned(half, ArenaAlign(arena))) {
        /* .split */
        Addr mid = AddrAdd(base, half);
        Seg segLo, segHi;
        Res sres;
        AMSUnallocateRange(seg, mid, limit);
        sres = SegSplit(&segLo, &segHi, seg, mid, withReservoirPermit);        
        if(ResOK == sres) { /* successful split */
          limit = mid;  /* range is lower segment */
        } else {            /* failed to split */
          AVER(amst->failSegs); /* deliberate fails only */
          AMSAllocateRange(seg, mid, limit);
        }

      }
    }
  }

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* AMSTStressBufferedSeg -- Stress test for a buffered seg
 *
 * Test splitting or merging a buffered seg.
 *
 * .bmerge: A merge is performed when the segment had previously
 * been split and the segment above meets the constraints (i.e. empty,
 * not already attached to a buffer and similar colour)
 *
 * .bsplit: Whether or not a merge happpened, a split is performed if 
 * the limit of the buffered region is arena aligned, and yet does not
 * correspond to the segment limit, provided that the part of the segment
 * above the buffer is all free.
 */

static void AMSTStressBufferedSeg(Seg seg, Buffer buffer)
{
  AMSTSeg amstseg;
  AMST amst;
  Arena arena;
  Addr limit;

  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
  AVER(SegBuffer(seg) == buffer);
  amstseg = SegAMSTSeg(seg);
  AVERT(AMSTSeg, amstseg);
  limit = BufferLimit(buffer);
  arena = PoolArena(SegPool(seg));
  amst = PoolPoolAMST(SegPool(seg));
  AVERT(AMST, amst);

  if(amstseg->next != NULL) {
    Seg segHi = AMSTSegSeg(amstseg->next);
    if(AMSSegIsFree(segHi) && SegGrey(segHi) == SegGrey(seg)) {
      /* .bmerge */
      Seg mergedSeg;
      Res res;
      res = SegMerge(&mergedSeg, seg, segHi, FALSE);
      if(ResOK == res) {
        amst->bmerges++;
        printf("J");
      } else {
        /* deliberate fails only */
        AVER(amst->failSegs); 
      }
    }
  }

  if (SegLimit(seg) != limit && 
      AddrIsAligned(limit, ArenaAlign(arena)) &&
      AMSSegRegionIsFree(seg, limit, SegLimit(seg))) {
    /* .bsplit */
    Seg segLo, segHi;
    Res res;
    res = SegSplit(&segLo, &segHi, seg, limit, FALSE);
    if(ResOK == res) {
      amst->bsplits++;
      printf("C");
    } else {
      /* deliberate fails only */
      AVER(amst->failSegs); 
    }
  }
}



/* AMSTPoolClass -- the pool class definition */

DEFINE_POOL_CLASS(AMSTPoolClass, this)
{
  INHERIT_CLASS(this, AMSPoolClass);
  this->name = "AMST";
  this->size = sizeof(AMSTStruct);
  this->offset = offsetof(AMSTStruct, amsStruct.poolStruct);
  this->init = AMSTInit;
  this->finish = AMSTFinish;
  this->bufferFill = AMSTBufferFill;
}


/* mps_amst_ap_stress -- stress an active buffer
 *
 * Attempt to either split or merge a segment attached to an AP
 */

static void mps_amst_ap_stress(mps_ap_t ap)
{
  Buffer buffer;
  Seg seg;

  buffer = BufferOfAP((AP)ap);
  AVERT(Buffer, buffer);
  seg = BufferSeg(buffer);
  AMSTStressBufferedSeg(seg, buffer);
}


/* mps_class_amst -- return the pool class descriptor to the client */

static mps_class_t mps_class_amst(void)
{
  return (mps_class_t)EnsureAMSTPoolClass();
}


#define exactRootsCOUNT 50
#define ambigRootsCOUNT 100
#define sizeScale       4
/* This is enough for five GCs. */ 
#define totalSizeMAX    sizeScale * 800 * (size_t)1024
#define totalSizeSTEP   200 * (size_t)1024
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)0xDECEA5ED)
#define testArenaSIZE   ((size_t)16<<20)
#define initTestFREQ    6000
#define stressTestFREQ  40


static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static size_t totalSize = 0;


static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
    if(res)
      die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  totalSize += size;
  return p;
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  size_t lastStep = 0, i, r;
  unsigned long objs;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;
  char *indent = "    ";

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(mps_fmt_create_A(&format, arena, dylan_fmt_A()), "fmt_create");

  die(mps_pool_create(&pool, arena, mps_class_amst(), format),
      "pool_create(amst)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i = 0; i < ambigRootsCOUNT; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   MPS_RANK_EXACT, (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");

  printf(indent);

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  objs = 0;
  while(totalSize < totalSizeMAX) {
    if(totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize;
      printf("\nSize %lu bytes, %lu objects.\n",
             (unsigned long)totalSize, objs);
      printf(indent);
      fflush(stdout);
      for(i = 0; i < exactRootsCOUNT; ++i)
        cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
             "all roots check");
    }

    r = (size_t)rnd();
    if(r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if(exactRoots[i] != objNULL)
        cdie(dylan_check(exactRoots[i]), "dying root check");
      exactRoots[i] = make();
      if(exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make();
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if(rnd() % stressTestFREQ == 0)
      mps_amst_ap_stress(ap); /* stress active buffer */

    if(rnd() % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    ++objs;
    if (objs % 256 == 0) {
      printf(".");
      fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

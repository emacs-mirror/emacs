/* impl.c.poolmfs: MANUAL FIXED SMALL UNIT POOL
 *
 * $HopeName: MMsrc!poolmfs.c(MMdevel_lib.3) $
 * Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 * This is the implementation of the MFS pool class.  MFS operates
 * in a very simple manner: each segment is divided into units.  Free
 * units are kept on a linked list using a header stored in the unit
 * itself.  The linked list it not ordered, so allocation and
 * deallocation simply pop and push from the head of the list.  This is
 * fast, but successive allocations might have poor locality if
 * previous successive frees did.
 *
 * **** RESTRICTION: This pool cannot allocate from the arena control
 * pool, nor can it allocate sub-pools, as it is used in the arena
 * bootstrap sequence.  See the arena manager implementation for
 * details.
 *
 * Notes
 *  1. The simple freelist policy might lead to poor locality of
 *     allocation if the list gets fragmented.  richard 1994-11-03
 *  2. free should check that the pointer it is asked to free is in a
 *     segment owned by the pool.  This required more support from the
 *     arena manager than is currently available.  richard 1994-11-03
 *  3. A describe method is needed.  richard 1994-11-03
 *  4. By not using the rounded extent of a segment some space may be
 *     wasted at the end in alloc.  richard 1994-11-03
 *  5. isValid should check that free list points into the pool.
 *     richard 1994-11-03
 *  6. This pool doesn't support fast cache allocation, which is a
 *     shame. richard 1994-11-03
 */

#include "mpm.h"
#include "poolmfs.h"

SRCID(poolmfs, "$HopeName: MMsrc!poolmfs.c(MMdevel_lib.3) $");


/*  == Round up ==
 *
 *  Rounds n up to the nearest multiple of unit.
 */

#define ROUND(unit, n)  ((n)+(unit)-1 - ((n)+(unit)-1)%(unit))

#define PoolPoolMFS(pool)       PARENT(MFSStruct, poolStruct, pool)


/*  == Free List Structure ==
 *
 *  The pool keeps a simple linked list of free units stored in the units
 *  themselves.  See note 1.
 */

typedef struct MFSHeaderStruct {
  struct MFSHeaderStruct *next;
} HeaderStruct, *Header;



#define UNIT_MIN        sizeof(HeaderStruct)

MFSInfo MFSGetInfo(void)
{
  static const struct MFSInfoStruct info =
  {
    /* unitSizeMin */   UNIT_MIN
  };
  return &info;
}


Pool (MFSPool)(MFS mfs)
{
  AVERT(MFS, mfs);
  return &mfs->poolStruct;
}


static Res MFSInit(Pool pool, va_list arg)
{
  Size extendBy, unitSize;
  MFS mfs;
  Space space;

  AVER(pool != NULL);

  extendBy = va_arg(arg, Size);
  unitSize = va_arg(arg, Size);

  AVER(unitSize >= UNIT_MIN);
  AVER(extendBy >= unitSize);
  
  mfs = PoolPoolMFS(pool);
  space = PoolSpace(pool);

  mfs->unroundedUnitSize = unitSize;

  unitSize = SizeAlignUp(unitSize, ARCH_ALIGN);
  extendBy = SizeAlignUp(extendBy, ArenaAlign(space));

  mfs->extendBy = extendBy;
  mfs->unitSize = unitSize;
  mfs->unitsPerSeg = extendBy/unitSize;
  mfs->freeList = NULL;
  mfs->segList = (Seg)0;
  mfs->sig = MFSSig;

  AVERT(MFS, mfs);

  return ResOK;
}


static void MFSFinish(Pool pool)
{
  Seg seg;
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  seg = mfs->segList;
  while(seg != NULL) {
    Seg nextSeg = (Seg)seg->p;
    PoolSegFree(pool, seg);
    seg = nextSeg;
  }

  mfs->sig = SigInvalid;
}


/*  == Allocate ==
 *
 *  Allocation simply involves taking a unit from the front of the freelist
 *  and returning it.  If there are none, a new segment is allocated.
 */

static Res MFSAlloc(Addr *pReturn, Pool pool, Size size)
{
  Header f;
  Res res;
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  AVER(pReturn != NULL);
  AVER(size == mfs->unroundedUnitSize);

  f = mfs->freeList;

  /* If the free list is empty then extend the pool with a new segment. */

  if(f == NULL)
  {
    Seg seg;
    Word i, unitsPerSeg, unitSize;
    Addr base;
    Header header = NULL, next;
    Space space;

    /* Create a new segment and attach it to the pool. */
    res = PoolSegAlloc(&seg, pool, mfs->extendBy);
    if(res != ResOK)
      return res;

    /* chain segs through seg->p; can find them when finishing */
    seg->p = (void *)mfs->segList;
    mfs->segList = seg;

    /* Sew together all the new empty units in the segment, working down */
    /* from the top so that they are in ascending order of address on the */
    /* free list. */

    space = PoolSpace(pool);
    unitsPerSeg = mfs->unitsPerSeg;
    unitSize = mfs->unitSize;
    base = SegBase(space, seg);
    next = NULL;

#define SUB(b, s, i)    ((Header)AddrAdd(b, (s)*(i)))

    for(i=0; i<unitsPerSeg; ++i)
    {
      header = SUB(base, unitSize, unitsPerSeg-i - 1);
      AVER(AddrIsAligned(header, pool->alignment));
      AVER(AddrAdd((Addr)header, unitSize) <= SegLimit(space, seg));
      header->next = next;
      next = header;
    }

#undef SUB

    /* The first unit in the segment is now the head of the new free list. */
    f = header;
  }

  AVER(f != NULL);

  /* Detach the first free unit from the free list and return its address. */

  mfs->freeList = f->next;

  *pReturn = (Addr)f;
  return ResOK;
}


/*  == Free ==
 *
 *  Freeing a unit simply involves pushing it onto the front of the
 *  freelist.  This might cause bad locality if units are pushed at random
 *  locations throughout the pool.
 */

static void MFSFree(Pool pool, Addr old, Size size)
{
  Header h;
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  AVER(old != (Addr)0);
  AVER(size == mfs->unroundedUnitSize);

  /* Locality isn't too good, but deallocation is quick.  See note 2. */
  h = (Header)old;
  h->next = mfs->freeList;
  mfs->freeList = h;
}


static Res MFSDescribe(Pool pool, mps_lib_FILE *stream)
{
  MFS mfs;
  Res res;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  AVER(stream != NULL);

  res = WriteF(stream,
               "  unrounded unit size $W\n", (Word)mfs->unroundedUnitSize,
               "  unit size $W\n",           (Word)mfs->unitSize,
               "  segment size $W\n",        (Word)mfs->extendBy,
               "  units per segment $U\n",   (unsigned long)mfs->unitsPerSeg,
               "  free list begins at $P\n", (void *)mfs->freeList,
               "  seg list begin at $P\n",   (void *)mfs->segList,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}


static PoolClassStruct PoolClassMFSStruct = {
  PoolClassSig,
  "MFS",                                /* name */
  sizeof(MFSStruct),                    /* size */
  offsetof(MFSStruct, poolStruct),      /* offset */
  AttrALLOC | AttrFREE,                 /* attr */
  MFSInit,                              /* init */
  MFSFinish,                            /* finish */
  MFSAlloc,                             /* alloc */
  MFSFree,                              /* free */
  PoolNoBufferInit,                     /* bufferInit */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferTrip,                     /* bufferTrip */
  PoolNoBufferExpose,                   /* bufferExpose */
  PoolNoBufferCover,                    /* bufferCover */
  PoolNoCondemn,                        /* condemn */
  PoolNoGrey,                           /* grey */
  PoolNoScan,                           /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* relcaim */
  PoolNoAccess,                         /* access */
  MFSDescribe,                          /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};


PoolClass PoolClassMFS(void)
{
  return &PoolClassMFSStruct;
}

Bool MFSCheck(MFS mfs)
{
  Space space;
  CHECKS(MFS, mfs);
  CHECKD(Pool, &mfs->poolStruct);
  CHECKL(mfs->poolStruct.class == &PoolClassMFSStruct);
  CHECKL(mfs->unroundedUnitSize >= UNIT_MIN);
  CHECKL(SizeAlignUp(mfs->unroundedUnitSize, mfs->poolStruct.alignment) == mfs->unitSize);
  CHECKL(mfs->extendBy >= UNIT_MIN);
  space = PoolSpace(&mfs->poolStruct);
  CHECKL(SizeIsAligned(mfs->extendBy, ArenaAlign(space)));
  CHECKL(mfs->unitsPerSeg == mfs->extendBy/mfs->unitSize);
  return TRUE;
}

/*  ==== MANUAL FIXED SMALL UNIT POOL ====
 *
 *  $HopeName: MMsrc/!poolmfs.c(trunk.4)$
 *
 *  Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the MFS pool class.  PoolMFS operates
 *  in a very simple manner: each segment is divided into units.  Free
 *  units are kept on a linked list using a header stored in the unit
 *  itself.  The linked list it not ordered, so allocation and
 *  deallocation simply pop and push from the head of the list.  This is
 *  fast, but successive allocations might have poor locality if
 *  previous successive frees did.
 *
 *  **** RESTRICTION: This pool cannot allocate from the arena control
 *  pool, nor can it allocate sub-pools, as it is used in the arena
 *  bootstrap sequence.  See the arena manager implementation for
 *  details.
 *
 *  Notes
 *   1. The simple freelist policy might lead to poor locality of allocation
 *      if the list gets fragmented.  richard 1994-11-03
 *   2. free should check that the pointer it is asked to free is in a
 *      segment owned by the pool.  This required more support from the
 *      arena manager than is currently available.  richard 1994-11-03
 *   3. A describe method is needed.  richard 1994-11-03
 *   4. By not using the rounded extent of a segment some space may be
 *      wasted at the end in alloc.  richard 1994-11-03
 *   5. isValid should check that free list points into the pool.
 *      richard 1994-11-03
 *   6. This pool doesn't support fast cache allocation, which is a shame.
 *      richard 1994-11-03
 */

#include "std.h"
#include "lib.h"
#include "deque.h"
#include "poolar.h"
#include "pool.h"
#include "poolst.h"
#include "poolmfs.h"
#include "poolmfss.h"
#include "trace.h"
#include <stdarg.h>
#include <stddef.h>


/*  == Round up ==
 *
 *  Rounds n up to the nearest multiple of unit.
 */

#define ROUND(unit, n)  ((n)+(unit)-1 - ((n)+(unit)-1)%(unit))


/*  == Class Structure ==  */

static Error create(Pool *poolReturn, Space space, va_list arg);
static void  destroy(Pool pool);
static Error alloc(Addr *pReturn, Pool pool, Size size);
static void free_(Pool pool, Addr old, Size size);
static Error describe(Pool pool, LibStream stream);

static PoolClassStruct PoolClassMFSStruct;

PoolClass PoolClassMFS(void)
{
  PoolClassInit(&PoolClassMFSStruct,
                "MFS",
                sizeof(PoolMFSStruct), offsetof(PoolMFSStruct, poolStruct),
                create, destroy,
                alloc, free_,
                NULL, NULL,		/* bufferCreate, bufferDestroy */
                NULL, NULL, NULL,	/* comdemn, mark, scan */
                NULL, NULL,		/* fix, relcaim */
                describe);
  return(&PoolClassMFSStruct);
}


/*  == Free List Structure ==
 *
 *  The pool keeps a simple linked list of free units stored in the units
 *  themselves.  See note 1.
 */

typedef struct PoolMFSHeaderStruct
{
  struct PoolMFSHeaderStruct *next;
} HeaderStruct, *Header;



#define UNIT_MIN        sizeof(HeaderStruct)

PoolMFSInfo PoolMFSGetInfo(void)
{
  static const struct PoolMFSInfoStruct info =
  {
    /* unitSizeMin */   UNIT_MIN
  };
  return(&info);
}


#ifdef DEBUG_ASSERT

Bool PoolMFSIsValid(PoolMFS poolMFS, ValidationType validParam)
{
  Arena arena;

  AVER(poolMFS != NULL);
  AVER(poolMFS->unroundedUnitSize >= UNIT_MIN);
  AVER(AlignUp(poolMFS->poolStruct.alignment, poolMFS->unroundedUnitSize) == poolMFS->unitSize);
  AVER(poolMFS->extendBy >= UNIT_MIN);
  arena = SpaceArena(PoolSpace(&poolMFS->poolStruct));
  AVER(IsAligned(ArenaGrain(arena), poolMFS->extendBy));
  AVER(poolMFS->unitsPerSeg == poolMFS->extendBy/poolMFS->unitSize);
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Pool (PoolMFSPool)(PoolMFS poolMFS)
{
  AVER(ISVALID(PoolMFS, poolMFS));
  return(&poolMFS->poolStruct);
}


Error PoolMFSCreate(PoolMFS *poolMFSReturn, Space space,
                    Size extendBy, Size unitSize)
{
  Error e;
  PoolMFS poolMFS;

  AVER(poolMFSReturn != NULL);
  AVER(ISVALID(Space, space));

  e = PoolAlloc((Addr *)&poolMFS, SpaceControlPool(space),
                 sizeof(PoolMFSStruct));
  if(e != ErrSUCCESS)
    return e;
  
  e = PoolMFSInit(poolMFS, space, extendBy, unitSize);
  if(e != ErrSUCCESS) {
    PoolFree(SpaceControlPool(space), (Addr)poolMFS, sizeof(PoolMFSStruct));
    return e;
  }
  
  *poolMFSReturn = poolMFS;
  return(ErrSUCCESS);
}

static Error create(Pool *poolReturn, Space space, va_list arg)
{
  Size extendBy, unitSize;
  PoolMFS poolMFS;
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  
  extendBy = va_arg(arg, Size);
  unitSize = va_arg(arg, Size);
  
  e = PoolMFSCreate(&poolMFS, space, extendBy, unitSize);
  if(e != ErrSUCCESS) return(e);
  
  *poolReturn = PoolMFSPool(poolMFS);
  return(ErrSUCCESS);
}


void PoolMFSDestroy(PoolMFS poolMFS)
{
  Pool control;
  AVER(ISVALID(PoolMFS, poolMFS));
  control = SpaceControlPool(PoolSpace(PoolMFSPool(poolMFS)));
  PoolMFSFinish(poolMFS);
  PoolFree(control, (Addr)poolMFS, sizeof(PoolMFSStruct));
}

static void destroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMFSStruct);
  PoolMFSDestroy(PARENT(PoolMFSStruct, poolStruct, pool));
}


Error PoolMFSInit(PoolMFS poolMFS, Space space, Size extendBy,
                  Size unitSize)
{
  AVER(poolMFS != NULL);
  AVER(ISVALID(Space, space));

  AVER(unitSize >= UNIT_MIN);
  AVER(extendBy >= unitSize);

  PoolInit(&poolMFS->poolStruct, space, PoolClassMFS());

  poolMFS->unroundedUnitSize = unitSize;

  unitSize = AlignUp(ARCH_ALIGNMOD, unitSize);
  extendBy = AlignUp(ArenaGrain(SpaceArena(space)), extendBy);

  poolMFS->extendBy = extendBy;
  poolMFS->unitSize = unitSize;
  poolMFS->unitsPerSeg = extendBy/unitSize;
  poolMFS->freeList = NULL;
  poolMFS->segList = (Addr)0;

  AVER(ISVALID(PoolMFS, poolMFS));

  return(ErrSUCCESS);
}


void PoolMFSFinish(PoolMFS poolMFS)
{
  Addr seg;
  Arena arena;
  Pool pool;

  AVER(ISVALID(PoolMFS, poolMFS));
  pool = PoolMFSPool(poolMFS);
  arena = SpaceArena(PoolSpace(pool));

  seg = poolMFS->segList;
  while(seg != (Addr)0) {
    Addr nextSeg = (Addr)ArenaGet(arena, seg, ARENA_CLASS);
    PoolSegFree(pool, seg, poolMFS->extendBy);
    seg = nextSeg;
  }
  PoolFinish(&poolMFS->poolStruct);
}


/*  == Allocate ==
 *
 *  Allocation simply involves taking a unit from the front of the freelist
 *  and returning it.  If there are none, a new segment is allocated.
 */

static Error alloc(Addr *pReturn, Pool pool, Size size)
{
  Header f;
  Error e;
  PoolMFS MFS;

#ifndef DEBUG_ASSERT
  UNUSED(size);
#endif

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMFSStruct);
  AVER(pReturn != (Addr)0);

  MFS = PARENT(PoolMFSStruct, poolStruct, pool);

  AVER(size == MFS->unroundedUnitSize);

  f = MFS->freeList;

  /* If the free list is empty then extend the pool with a new segment. */

  if(f == NULL)
  {
    Addr seg;
    Addr i, unitsPerSeg, unitSize, base;
    Arena arena;
    Header header = NULL, next;

    /* Create a new segment and attach it to the pool. */
    e = PoolSegAlloc(&seg, pool, MFS->extendBy);
    if(e != ErrSUCCESS)
      return e;

    /* chain segs through Key1; can find them when finishing */
    arena = SpaceArena(PoolSpace(pool));
    ArenaPut(arena, seg, ARENA_CLASS, (void *)MFS->segList);
    MFS->segList = seg;

    /* Sew together all the new empty units in the segment, working down */
    /* from the top so that they are in ascending order of address on the */
    /* free list. */

    unitsPerSeg = MFS->unitsPerSeg;
    unitSize = MFS->unitSize;
    base = seg;
    next = NULL;

#define SUB(b, s, i)    ((Header)((b)+(s)*(i)))

    for(i=0; i<unitsPerSeg; ++i)
    {
      header = SUB(base, unitSize, unitsPerSeg-i - 1);
      AVER(IsAligned(pool->alignment, (Addr)header));
      AVER((Addr)header + unitSize <= seg + ArenaSegSize(arena, seg));
      header->next = next;
      next = header;
    }

#undef SUB

    /* The first unit in the segment is now the head of the new free list. */
    f = header;
  }

  AVER(f != NULL);

  /* Detach the first free unit from the free list and return its address. */

  MFS->freeList = f->next;

  *pReturn = (Addr)f;
  return ErrSUCCESS;
}


/*  == Free ==
 *
 *  Freeing a unit simply involves pushing it onto the front of the
 *  freelist.  This might cause bad locality if units are pushed at random
 *  locations throughout the pool.
 */

static void free_(Pool pool, Addr old, Size size)
{
  Header h;
  PoolMFS MFS;

#ifndef DEBUG_ASSERT
  UNUSED(size);
#endif

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMFSStruct);
  AVER(old != (Addr)0);

  MFS = PARENT(PoolMFSStruct, poolStruct, pool);

  AVER(size == MFS->unroundedUnitSize);

  /* Locality isn't too good, but deallocation is quick.  See note 2. */
  h = (Header)old;
  h->next = MFS->freeList;
  MFS->freeList = h;
}


static Error describe(Pool pool, LibStream stream)
{
  PoolMFS MFS;
  Error e;

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMFSStruct);
  AVER(stream != NULL);

  MFS = PARENT(PoolMFSStruct, poolStruct, pool);

  e = LibFormat(stream,
                "  unrounded unit size %lu\n"
                "  unit size %lu  segment size %lu\n"
                "  units per segment %u\n"
                "  free list begins at %p\n"
                "  seg list begin at %08lx\n",
                (unsigned long)MFS->unroundedUnitSize,
                (unsigned long)MFS->unitSize,
                (unsigned long)MFS->extendBy,
                MFS->unitsPerSeg,
                MFS->freeList,
                MFS->segList);

  return(e);
}


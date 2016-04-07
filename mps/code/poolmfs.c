/* poolmfs.c: MANUAL FIXED SMALL UNIT POOL
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This is the implementation of the MFS pool class.
 *
 * DESIGN
 *
 * .design.misplaced: This design is misplaced, it should be in a
 * separate document.
 *
 * MFS operates in a very simple manner: each region allocated from
 * the arena is divided into units.  Free units are kept on a linked
 * list using a header stored in the unit itself.  The linked list is
 * not ordered; allocation anddeallocation simply pop and push from
 * the head of the list.  This is fast, but successive allocations might
 * have poor locality if previous successive frees did.
 *
 * .restriction: This pool cannot allocate from the arena control
 * pool (as the control pool is an instance of PoolClassMV and MV uses
 * MFS in its implementation), nor can it allocate sub-pools, as that
 * causes allocation in the control pool.
 *
 * Notes
 *
 * .freelist.fragments: The simple freelist policy might lead to poor
 * locality of allocation if the list gets fragmented.
 *
 * .buffer.not: This pool doesn't support fast cache allocation, which
 * is a shame.
 */

#include "mpscmfs.h"
#include "dbgpool.h"
#include "poolmfs.h"
#include "mpm.h"

SRCID(poolmfs, "$Id$");


/* ROUND -- Round up
 *
 *  Rounds n up to the nearest multiple of unit.
 */

#define ROUND(unit, n)  ((n)+(unit)-1 - ((n)+(unit)-1)%(unit))


#define PoolPoolMFS(pool)       PARENT(MFSStruct, poolStruct, pool)


/* HeaderStruct -- Freelist structure */

typedef struct MFSHeaderStruct {
  struct MFSHeaderStruct *next;
} HeaderStruct, *Header;


#define UNIT_MIN        sizeof(HeaderStruct)


/* MFSVarargs -- decode obsolete varargs */

static void MFSVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_EXTEND_BY;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_MFS_UNIT_SIZE;
  args[1].val.size = va_arg(varargs, Size);
  args[2].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}

ARG_DEFINE_KEY(MFS_UNIT_SIZE, Size);
ARG_DEFINE_KEY(MFSExtendSelf, Bool);

static Res MFSInit(Pool pool, ArgList args)
{
  Size extendBy = MFS_EXTEND_BY_DEFAULT;
  Bool extendSelf = TRUE;
  Size unitSize;
  MFS mfs;
  Arena arena;
  ArgStruct arg;

  AVER(pool != NULL);
  AVERT(ArgList, args);
  
  ArgRequire(&arg, args, MPS_KEY_MFS_UNIT_SIZE);
  unitSize = arg.val.size;
  if (ArgPick(&arg, args, MPS_KEY_EXTEND_BY))
    extendBy = arg.val.size;
  if (ArgPick(&arg, args, MFSExtendSelf))
    extendSelf = arg.val.b;

  AVER(unitSize > 0);
  AVER(extendBy > 0);
  AVERT(Bool, extendSelf);
 
  mfs = PoolPoolMFS(pool);
  arena = PoolArena(pool);

  mfs->unroundedUnitSize = unitSize;

  if (unitSize < UNIT_MIN)
    unitSize = UNIT_MIN;
  unitSize = SizeAlignUp(unitSize, MPS_PF_ALIGN);
  if (extendBy < unitSize)
    extendBy = unitSize;
  extendBy = SizeArenaGrains(extendBy, arena);

  mfs->extendBy = extendBy;
  mfs->extendSelf = extendSelf;
  mfs->unitSize = unitSize;
  mfs->freeList = NULL;
  mfs->tractList = NULL;
  mfs->total = 0;
  mfs->free = 0;
  mfs->sig = MFSSig;

  AVERT(MFS, mfs);
  EVENT5(PoolInitMFS, pool, arena, extendBy, BOOLOF(extendSelf), unitSize);
  return ResOK;
}


void MFSFinishTracts(Pool pool, MFSTractVisitor visitor,
                     void *closure)
{
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);
  
  while (mfs->tractList != NULL) {
    Tract nextTract = (Tract)TractP(mfs->tractList);   /* .tract.chain */
    visitor(pool, TractBase(mfs->tractList), mfs->extendBy, closure);
    mfs->tractList = nextTract;
  }
}


static void MFSTractFreeVisitor(Pool pool, Addr base, Size size,
                                void *closure)
{
  AVER(closure == UNUSED_POINTER);
  UNUSED(closure);
  ArenaFree(base, size, pool);
}


static void MFSFinish(Pool pool)
{
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  MFSFinishTracts(pool, MFSTractFreeVisitor, UNUSED_POINTER);

  mfs->sig = SigInvalid;
}


void MFSExtend(Pool pool, Addr base, Size size)
{
  MFS mfs;
  Tract tract;
  Word i, unitsPerExtent;
  Size unitSize;
  Header header = NULL;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);
  AVER(size == mfs->extendBy);

  /* Ensure that the memory we're adding belongs to this pool.  This is
     automatic if it was allocated using ArenaAlloc, but if the memory is
     being inserted from elsewhere then it must have been set up correctly. */
  AVER(PoolHasAddr(pool, base));
  
  /* .tract.chain: chain first tracts through TractP(tract) */
  tract = TractOfBaseAddr(PoolArena(pool), base);

  AVER(TractPool(tract) == pool);

  TractSetP(tract, (void *)mfs->tractList);
  mfs->tractList = tract;

  /* Update accounting */
  mfs->total += size;
  mfs->free += size;

  /* Sew together all the new empty units in the region, working down */
  /* from the top so that they are in ascending order of address on the */
  /* free list. */

  unitSize = mfs->unitSize;
  unitsPerExtent = size/unitSize;
  AVER(unitsPerExtent > 0);

#define SUB(b, s, i)    ((Header)AddrAdd(b, (s)*(i)))

  for(i = 0; i < unitsPerExtent; ++i)
  {
    header = SUB(base, unitSize, unitsPerExtent-i - 1);
    AVER(AddrIsAligned(header, pool->alignment));
    AVER(AddrAdd((Addr)header, unitSize) <= AddrAdd(base, size));
    header->next = mfs->freeList;
    mfs->freeList = header;
  }

#undef SUB
}


/*  == Allocate ==
 *
 *  Allocation simply involves taking a unit from the front of the freelist
 *  and returning it.  If there are none, a new region is allocated from the
 *  arena.
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

  /* If the free list is empty then extend the pool with a new region. */

  if(f == NULL)
  {
    Addr base;

    /* See design.mps.bootstrap.land.sol.pool. */
    if (!mfs->extendSelf)
      return ResLIMIT;

    /* Create a new region and attach it to the pool. */
    res = ArenaAlloc(&base, LocusPrefDefault(), mfs->extendBy, pool);
    if(res != ResOK)
      return res;

    MFSExtend(pool, base, mfs->extendBy);

    /* The first unit in the region is now the head of the new free list. */
    f = mfs->freeList;
  }

  AVER(f != NULL);

  /* Detach the first free unit from the free list and return its address. */

  mfs->freeList = f->next;
  AVER(mfs->free >= mfs->unitSize);
  mfs->free -= mfs->unitSize;

  *pReturn = (Addr)f;
  return ResOK;
}


/*  == Free ==
 *
 *  Freeing a unit simply involves pushing it onto the front of the
 *  freelist.
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

  /* .freelist.fragments */
  h = (Header)old;
  h->next = mfs->freeList;
  mfs->freeList = h;
  mfs->free += mfs->unitSize;
}


/* MFSTotalSize -- total memory allocated from the arena */

static Size MFSTotalSize(Pool pool)
{
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  return mfs->total;
}


/* MFSFreeSize -- free memory (unused by client program) */

static Size MFSFreeSize(Pool pool)
{
  MFS mfs;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  return mfs->free;
}


static Res MFSDescribe(Pool pool, mps_lib_FILE *stream, Count depth)
{
  MFS mfs;
  Res res;

  AVERT(Pool, pool);
  mfs = PoolPoolMFS(pool);
  AVERT(MFS, mfs);

  AVER(stream != NULL);

  res = WriteF(stream, depth,
               "unroundedUnitSize $W\n", (WriteFW)mfs->unroundedUnitSize,
               "extendBy $W\n", (WriteFW)mfs->extendBy,
               "extendSelf $S\n", WriteFYesNo(mfs->extendSelf),
               "unitSize $W\n", (WriteFW)mfs->unitSize,
               "freeList $P\n", (WriteFP)mfs->freeList,
               "total $W\n", (WriteFW)mfs->total,
               "free $W\n", (WriteFW)mfs->free,
               "tractList $P\n", (WriteFP)mfs->tractList,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


DEFINE_POOL_CLASS(MFSPoolClass, this)
{
  INHERIT_CLASS(this, MFSPoolClass, AbstractPoolClass);
  this->size = sizeof(MFSStruct);
  this->offset = offsetof(MFSStruct, poolStruct);
  this->varargs = MFSVarargs;
  this->init = MFSInit;
  this->finish = MFSFinish;
  this->alloc = MFSAlloc;
  this->free = MFSFree;
  this->totalSize = MFSTotalSize;
  this->freeSize = MFSFreeSize;  
  this->describe = MFSDescribe;
  AVERT(PoolClass, this);
}


PoolClass PoolClassMFS(void)
{
  return CLASS(MFSPoolClass);
}


mps_pool_class_t mps_class_mfs(void)
{
  return (mps_pool_class_t)PoolClassMFS();
}


Bool MFSCheck(MFS mfs)
{
  Arena arena;

  CHECKS(MFS, mfs);
  CHECKD(Pool, MFSPool(mfs));
  CHECKL(MFSPool(mfs)->class == CLASS(MFSPoolClass));
  CHECKL(mfs->unitSize >= UNIT_MIN);
  CHECKL(mfs->extendBy >= UNIT_MIN);
  CHECKL(BoolCheck(mfs->extendSelf));
  arena = PoolArena(MFSPool(mfs));
  CHECKL(SizeIsArenaGrains(mfs->extendBy, arena));
  CHECKL(SizeAlignUp(mfs->unroundedUnitSize, PoolAlignment(MFSPool(mfs))) ==
         mfs->unitSize);
  if(mfs->tractList != NULL) {
    CHECKD_NOSIG(Tract, mfs->tractList);
  }
  CHECKL(mfs->free <= mfs->total);
  CHECKL((mfs->total - mfs->free) % mfs->unitSize == 0);
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

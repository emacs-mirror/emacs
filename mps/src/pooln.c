/*  impl.c.pooln
 *
 *                         NULL POOL
 *
 *  $HopeName: MMsrc/!pooln.c(trunk.1)$
 *
 *  Copyright(C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the null pool class.  Begin null it
 *  all functions are implemented in a trivial manner.
 */


#include "std.h"
#include "pooln.h"
#include "poolnst.h"
#include "buffer.h"
#include "lib.h"
#include "pool.h"
#include "poolar.h"
#include "poolclas.h"
#include "ref.h"
#include "space.h"
#include "trace.h"
#include "prot.h"

#include <stdarg.h>
#include <stddef.h>


/*  Class's methods  */

static Error create(Pool *poolReturn, Space space, va_list arg);
static void destroy(Pool pool);
static Error alloc(Addr *pReturn, Pool pool, Size size);
static void free_(Pool pool, Addr old, Size size);
static Error bufferCreate(Buffer *bufReturn, Pool pool);
static void bufferDestroy(Buffer buf);
static Error describe(Pool pool, LibStream stream);
static Error condemn(Pool pool, Trace trace);
static void mark(Pool pool, Trace trace);
static Error scan(Pool pool, Trace trace, RefRank rank);
static Error fix(Pool pool, Trace trace, RefRank rank,
                 Arena arena, Ref *refIO);
static void reclaim(Pool pool, Trace trace);
static void access(Pool pool, Addr seg, ProtMode mode);


/*  Class Structure  */
static PoolClassStruct PoolClassNStruct;


PoolClass PoolClassN(void)
{
  PoolClassInit(&PoolClassNStruct,
                "N",
                sizeof(PoolNStruct), offsetof(PoolNStruct, poolStruct),
                create, destroy,
                alloc, free_,
                bufferCreate, bufferDestroy,
                condemn, mark, scan,
                fix, reclaim,
                access,
                describe);
  return &PoolClassNStruct;
}


Bool PoolNIsValid(PoolN poolN, ValidationType validParam)
{
  AVER(poolN != NULL);
  AVER(ISVALIDNESTED(Pool, &poolN->poolStruct));
  AVER(poolN->poolStruct.class == &PoolClassNStruct);

  return TRUE;
}


static Error create(Pool *poolReturn, Space space, va_list arg)
{
  PoolN poolN;
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  UNUSED(arg);

  e = PoolNCreate(&poolN, space);
  if(e != ErrSUCCESS) {
    return e;
  }
  *poolReturn = PoolNPool(poolN);
  return ErrSUCCESS;
}

Error PoolNCreate(PoolN *poolNReturn, Space space)
{
  Error e;
  Pool control;
  PoolN poolN;

  AVER(poolNReturn != NULL);
  AVER(ISVALID(Space, space));

  control = SpaceControlPool(space);
  e = PoolAlloc((Addr *)&poolN, control, (Size)sizeof(PoolNStruct));
  if(e != ErrSUCCESS)
    goto failAlloc;
  e = PoolNInit(poolN, space);
  if(e != ErrSUCCESS)
    goto failInit;

  *poolNReturn = poolN;
  return ErrSUCCESS;

  failInit:
    PoolFree(control, (Addr)poolN, (Size)sizeof(PoolNStruct));
  failAlloc:
    return e;
}

Error PoolNInit(PoolN poolN, Space space)
{
  AVER(poolN != NULL);
  AVER(ISVALID(Space, space));

  PoolInit(&poolN->poolStruct, space, PoolClassN());
  AVER(ISVALID(PoolN, poolN));

  return ErrSUCCESS;
}

static void destroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  PoolNDestroy(PARENT(PoolNStruct, poolStruct, pool));
}

void PoolNDestroy(PoolN poolN)
{
  Pool control;

  AVER(ISVALID(PoolN, poolN));
  control = SpaceControlPool(PoolSpace(PoolNPool(poolN)));
  PoolNFinish(poolN);
  PoolFree(control, (Addr)poolN, (Size)sizeof(PoolNStruct));
}

void PoolNFinish(PoolN poolN)
{
  AVER(ISVALID(PoolN, poolN));
  PoolFinish(PoolNPool(poolN));
}

Pool (PoolNPool)(PoolN poolN)
{
  AVER(ISVALID(PoolN, poolN));
  return &poolN->poolStruct;
}

static Error alloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(size > 0);
#ifndef DEBUG_ASSERT
  UNUSED(pReturn);
  UNUSED(pool);
  UNUSED(size);
#endif /* DEBUG_ASSERT */

  return ErrLIMIT;  /* limit of nil blocks exceeded */
}

static void free_(Pool pool, Addr old, Size size)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(old != NULL);
  AVER(size > 0);
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(old);
  UNUSED(size);
#endif /* DEBUG_ASSERT */

  NOTREACHED;  /* can't allocate, should never free */
}

static Error bufferCreate(Buffer *bufReturn, Pool pool)
{
  AVER(bufReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
#ifndef DEBUG_ASSERT
  UNUSED(bufReturn);
  UNUSED(pool);
#endif /* DEBUG_ASSERT */

  return ErrLIMIT;  /* limit of nil buffers exceeded */
}

static void bufferDestroy(Buffer buf)
{
  AVER(ISVALID(Buffer, buf));
#ifndef DEBUG_ASSERT
  UNUSED(buf);
#endif /* DEBUG_ASSERT */

  NOTREACHED;  /* can't create, so shouldn't destroy */
}


static Error describe(Pool pool, LibStream stream)
{
  PoolN poolN;

  UNUSED(stream);

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);

  poolN = PARENT(PoolNStruct, poolStruct, pool);
  AVER(ISVALID(PoolN, poolN));
#ifndef DEBUG_ASSERT
  UNUSED(poolN);
#endif /* DEBUG_ASSERT */

  return ErrSUCCESS;
}


static Error condemn(Pool pool, Trace trace)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Trace, trace));
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(trace);
#endif /* DEBUG_ASSERT */

  return ErrSUCCESS;
}

static void mark(Pool pool, Trace trace)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Trace, trace));
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(trace);
#endif
}

static Error scan(Pool pool, Trace trace, RefRank rank)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(trace);
  UNUSED(rank);
#endif /* DEBUG_ASSERT */

  return ErrSUCCESS;
}

static Error fix(Pool pool, Trace trace, RefRank rank,
                 Arena arena, Ref *refIO)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
  AVER(ISVALID(Arena, arena));
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(trace);
  UNUSED(rank);
  UNUSED(arena);
  UNUSED(refIO);
#endif /* DEBUG_ASSERT */
  NOTREACHED;  /* since we don't allocate any objects, should never
                * be called upon to fix a reference */
  return ErrFAILURE;
}

static void reclaim(Pool pool, Trace trace)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  AVER(ISVALID(Trace, trace));
#ifndef DEBUG_ASSERT
  UNUSED(pool);
  UNUSED(trace);
#endif
  /* all unmarked and condemned objects reclaimed */
}

static void access(Pool pool, Addr seg, ProtMode mode)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassNStruct);
  UNUSED(seg);
  UNUSED(mode);
#ifndef DEBUG_ASSERT
  UNUSED(pool);
#endif
  /* deal with access to segment */
}

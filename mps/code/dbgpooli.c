/* impl.c.dbgpooli: POOL DEBUG MIXIN C INTERFACE
 *
 * $Id: dbgpooli.c,v 1.3 2002/02/01 14:27:26 pekka Exp $
 * $HopeName: MMsrc!dbgpooli.c(trunk.3) $
 * Copyright (C) 2002 Global Graphics Software.
 *
 * .source: design.mps.object-debug
 */

#include "dbgpool.h"
#include "mps.h"
#include "mpm.h"

SRCID(dbgpooli, "$Id: dbgpooli.c,v 1.3 2002/02/01 14:27:26 pekka Exp $");


/* mps_pool_check_fenceposts -- check all the fenceposts in the pool */

void mps_pool_check_fenceposts(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  
  /* CHECKT not AVERT, see design.mps.interface.c.check.space */
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  DebugPoolCheckFences(pool);

  ArenaLeave(arena);
}


/* mps_pool_check_free_space -- check free space in the pool for overwrites */

void mps_pool_check_free_space(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  
  /* CHECKT not AVERT, see design.mps.interface.c.check.space */
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  DebugPoolCheckFreeSpace(pool);

  ArenaLeave(arena);
}

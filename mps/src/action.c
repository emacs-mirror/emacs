/* impl.c.action: STRATEGIC ACTION
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: !action.c(trunk.6) $
 */

#include "mpm.h"
#include <float.h> /* @@@@ for DBL_MAX */

SRCID(action, "$HopeName: !action.c(trunk.6) $");


/* ActionCheck -- check consistency of an Action structure */

Bool ActionCheck(Action action)
{
  CHECKS(Action, action);
  CHECKU(Pool, action->pool);
  CHECKL(RingCheck(&action->poolRing));
  CHECKL(action->serial <= action->pool->actionSerial);
  return TRUE;
}


/* ActionInit -- initialize an action structure */

void ActionInit(Action action, Pool pool)
{
  AVER(action != NULL);
  AVERT(Pool, pool);

  action->pool = pool;
  RingInit(&action->poolRing);

  action->sig = ActionSig;
  action->serial = pool->actionSerial;
  ++pool->actionSerial;

  AVERT(Action, action);

  RingAppend(&pool->actionRing, &action->poolRing);
}


/* ActionFinish -- finish an action structure */

void ActionFinish(Action action)
{
  AVERT(Action, action);

  RingRemove(&action->poolRing);

  action->sig = SigInvalid;

  RingFinish(&action->poolRing);
}


/* ActionPoll -- decide what to do next
 *
 * This is the brain of the system.  The function weighs up the
 * costs and benefits of the various actions exhibited by the pools,
 * and takes those which are worthwhile.
 */

Bool ActionPoll(Arena arena)
{
  Ring poolNode, nextPoolNode;
  double bestBenefit;
  Action bestAction;

  AVERT(Arena, arena);

  bestBenefit = -DBL_MAX;
  bestAction = NULL;

  RING_FOR(poolNode, &arena->poolRing, nextPoolNode) {
    Pool pool = RING_ELT(Pool, arenaRing, poolNode);
    Ring actionNode, nextActionNode;

    RING_FOR(actionNode, &pool->actionRing, nextActionNode) {
      Action action = RING_ELT(Action, poolRing, actionNode);
      double benefit;
      AVERT(Action, action);

      benefit = PoolBenefit(action->pool, action);
      if(benefit >= bestBenefit) {
        bestBenefit = benefit;
        bestAction = action;
      }
    }
  }

  /* @@@@ ignores failure */
  if(bestBenefit > 0) {
    AVER(bestAction != NULL);
    (void)PoolAct(bestAction->pool, bestAction);
    return TRUE;
  }
  return FALSE;
}

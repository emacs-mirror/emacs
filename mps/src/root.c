/*  ==== ROOTS ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of roots.
 */

#include "std.h"
#include "lib.h"
#include "deque.h"
#include "root.h"
#include "rootst.h"
#include "pool.h"
#include "fix.h"


#ifdef DEBUG_SIGN
static SigStruct RootSigStruct;
#endif


#ifdef DEBUG_ASSERT

static Bool modeIsValid(RootMode mode)
{
  AVER((mode &~(RootEXACT | RootFIXABLE | RootMUTABLE | RootATOMIC)) == 0);
  /* Need to check for legal combinations of modes. */
  return(TRUE);
}

#endif


Bool RootIsValid(Root root, ValidationType validParam)
{
  AVER(root != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &RootSigStruct));
  AVER(root->sig == &RootSigStruct);
#endif
  AVER(ISVALIDNESTED(DequeNode, &root->spaceDeque));
  AVER(modeIsValid(root->mode));
  AVER(root->type == RootTABLE || root->type == RootFUN);
  switch(root->type)
  {
    case RootTABLE:
    AVER(root->the.table.base != 0);
    AVER(root->the.table.base < root->the.table.limit);
    break;
    
    case RootFUN:
    AVER(root->the.fun.scan != NULL);
    break;

    default:
    NOTREACHED;
  }
  return(TRUE);
}


static Error create(Root *rootReturn, Pool pool, RootMode mode,
		    RootType type, RootUnion theUnion)
{
  Root root;
  Error e;

  AVER(rootReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(modeIsValid(mode));

  e = PoolAllocP((void **)&root, pool, sizeof(RootStruct));
  if(e != ErrSUCCESS) return(e);

  root->pool = pool;
  root->mode = mode;
  root->type = type;
  root->the  = theUnion;

  DequeNodeInit(&root->spaceDeque);

#ifdef DEBUG_SIGN
  SigInit(&RootSigStruct, "Root");
  root->sig = &RootSigStruct;
#endif

  AVER(ISVALID(Root, root));

  *rootReturn = root;
  return(ErrSUCCESS);
}


Error RootCreateTable(Root *rootReturn, Pool pool, RootMode mode,
                      Addr *base, Addr *limit)
{
  RootUnion theUnion;

  AVER(base != 0);
  AVER(base < limit);

  theUnion.table.base = base;
  theUnion.table.limit = limit;
  
  return(create(rootReturn, pool, mode, RootTABLE, theUnion));
}


Error RootCreateFun(Root *rootReturn, Pool pool, RootMode mode,
                    void (*scan)(void *p, int i, Fixes fixes),
                    void *p, int i)
{
  RootUnion theUnion;
  
  AVER(scan != NULL);

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.i = i;

  return(create(rootReturn, pool, mode, RootFUN, theUnion));
}


void RootDestroy(Root root)
{
  AVER(ISVALID(Root, root));

  DequeNodeFinish(&root->spaceDeque);

#ifdef DEBUG_SIGN
  root->sig = SigInvalid;
#endif

  PoolFreeP(root->pool, root, sizeof(RootStruct));
}


Bool RootIsExact(Root root)
{
  AVER(ISVALID(Root, root));

  if(root->mode & RootEXACT)
    return(TRUE);
  else
    return(FALSE);
}


void RootScan(Root root, Fixes fixes)
{
  AVER(ISVALID(Root, root));
  AVER(ISVALID(Fixes, fixes));

  switch(root->type)
  {
    case RootTABLE:
    {
      Addr *what, *limit;
      what = (Addr *)root->the.table.base;
      limit = (Addr *)root->the.table.limit;
      while(what < limit)
      {
	(void)FixesApply(fixes, what, *what);
	++what;
      }
    }
    break;

    case RootFUN:
    (*root->the.fun.scan)(root->the.fun.p, root->the.fun.i, fixes);
    break;

    default:
    NOTREACHED;
  }
}


Error RootDescribe(Root root, LibStream stream)
{
  AVER(ISVALID(Root, root));
  AVER(stream != NULL);
  
  LibFormat(stream,
          "Root %lX {\n"
          "  mode%s%s%s%s\n",
          (unsigned long)root,
          root->mode & RootEXACT   ? " EXACT"   : "",
          root->mode & RootATOMIC  ? " ATOMIC"  : "",
          root->mode & RootFIXABLE ? " FIXABLE" : "",
          root->mode & RootMUTABLE ? " MUTABLE" : "");
  
  switch(root->type)
  {
    case RootTABLE:
    LibFormat(stream, "  table base 0x%lX limit 0x%lX\n",
            (unsigned long)root->the.table.base,
            (unsigned long)root->the.table.limit);
    break;
    
    case RootFUN:
    LibFormat(stream,
            "  scan function 0x%lX\n"
            "  environment p 0x%lX i %d\n",
            (unsigned long)root->the.fun.scan,
            (unsigned long)root->the.fun.p,
            root->the.fun.i);
    break;

    default:
    NOTREACHED;
  }
  
  LibFormat(stream, "} Root 0x%lX\n", (unsigned long)root);
  
  return(ErrSUCCESS);
}

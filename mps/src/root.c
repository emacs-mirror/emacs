/* impl.c.root: ROOT IMPLEMENTATION
 *
 * $HopeName: MMsrc!root.c(trunk.22) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .scope: This is the implementation of the root datatype.
 * .design: For design, see design.mps.root and 
 * design.mps.root-interface
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(trunk.22) $");


/* RootVarCheck -- check a Root union discriminator
 *
 * .rootvarcheck: Synchronize with impl.h.mpmtypes.rootvar
 */

Bool RootVarCheck(RootVar rootVar)
{
  AVER(rootVar == RootTABLE ||
       rootVar == RootTABLE_MASKED ||
       rootVar == RootFUN ||
       rootVar == RootFMT ||
       rootVar == RootREG);
  return(TRUE);
}


/* RootCheck -- check the consistency of a root structure
 *
 * .rootcheck: Keep synchonized with impl.h.mpmst.root
 */

Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Arena, root->arena); 
  CHECKL(root->serial < root->arena->rootSerial);
  CHECKL(RingCheck(&root->arenaRing));
  CHECKL(RankCheck(root->rank));
  CHECKL(TraceSetCheck(root->grey));
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(root->the.table.base != 0);
    CHECKL(root->the.table.base < root->the.table.limit);
    break;

    case RootTABLE_MASKED:
    CHECKL(root->the.tableMasked.base != 0);
    CHECKL(root->the.tableMasked.base < root->the.tableMasked.limit);
    /* Can't check anything about the mask. */
    break;

    case RootFUN:
    CHECKL(root->the.fun.scan != NULL);
    break;

    case RootREG:
    CHECKL(root->the.reg.scan != NULL);
    CHECKD(Thread, root->the.reg.thread);
    break;

    case RootFMT:
    CHECKL(root->the.fmt.scan != NULL);
    CHECKL(root->the.fmt.base != 0);
    CHECKL(root->the.fmt.base < root->the.fmt.limit);
    break;

    default:
    NOTREACHED;
  }
  return TRUE;
}


/* .create: create, RootCreateTable, RootCreateReg, RootCreateFmt, 
 *   RootCreateFun:
 * RootCreate* set up the appropriate union member, and call the generic
 * create function to do the actual creation 
 * 
 * See design.mps.root.init for initial value
 */

static Res create(Root *rootReturn, Arena arena,
                  Rank rank, RootVar type,
                  union RootUnion *theUnionP)
{
  Root root;
  Res res;
  void *p;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  AVERT(RootVar, type);

  res = ArenaAlloc(&p, arena, sizeof(RootStruct));
  if(res != ResOK)
    return res;
  root = (Root)p; /* Avoid pun */

  root->arena = arena;
  root->rank = rank;
  root->var = type;
  root->the  = *theUnionP;
  root->grey = TraceSetEMPTY;
  root->summary = RefSetUNIV;

  /* See design.mps.arena.root-ring */
  RingInit(&root->arenaRing);

  root->serial = arena->rootSerial;
  ++arena->rootSerial;
  root->sig = RootSig;

  AVERT(Root, root);

  RingAppend(ArenaRootRing(arena), &root->arenaRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Arena arena,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  AVER(base != 0);
  AVER(base < limit);  

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return create(rootReturn, arena, rank, RootTABLE, &theUnion);
}

Res RootCreateTableMasked(Root *rootReturn, Arena arena,
                          Rank rank, Addr *base, Addr *limit,
                          Word mask)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  AVER(base != 0);
  AVER(base < limit);
  /* Can't check anything about mask. */

  theUnion.tableMasked.base = base;
  theUnion.tableMasked.limit = limit;
  theUnion.tableMasked.mask = mask;

  return create(rootReturn, arena, rank, RootTABLE_MASKED, &theUnion);
}

Res RootCreateReg(Root *rootReturn, Arena arena,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  AVERT(Thread, thread);
  AVER(scan != NULL);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;
  theUnion.reg.s = s;

  return create(rootReturn, arena, rank, RootREG, &theUnion);
}

Res RootCreateFmt(Root *rootReturn, Arena arena,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return create(rootReturn, arena, rank, RootFMT, &theUnion);
}

Res RootCreateFun(Root *rootReturn, Arena arena,
                 Rank rank,
                 RootScanMethod scan,
                 void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, arena, rank, RootFUN, &theUnion);
}

void RootDestroy(Root root)
{
  Arena arena;

  AVERT(Root, root);

  arena = RootArena(root);

  AVERT(Arena, arena);

  RingRemove(&root->arenaRing);
  RingFinish(&root->arenaRing);

  root->sig = SigInvalid;

  ArenaFree(arena, (Addr)root, sizeof(RootStruct));
}

Rank RootRank(Root root)
{
  AVERT(Root, root);
  return root->rank;
}

void RootGrey(Root root, Trace trace)
{
  AVERT(Root, root);
  AVERT(Trace, trace);
  
  root->grey = TraceSetAdd(root->grey, trace->ti);
}

Res RootScan(ScanState ss, Root root)
{
  Res res;

  AVERT(Root, root);
  AVERT(ScanState, ss);
  AVER(root->rank == ss->rank);

  if(TraceSetInter(root->grey, ss->traces) == TraceSetEMPTY)
    return ResOK;

  switch(root->var) {
    case RootTABLE:
    res = TraceScanArea(ss, root->the.table.base, root->the.table.limit);
    if(res != ResOK) return res;
    break;

    case RootTABLE_MASKED:
    res = TraceScanAreaMasked(ss,
                              root->the.tableMasked.base,
                              root->the.tableMasked.limit,
                              root->the.tableMasked.mask);
    if(res != ResOK) return res;
    break;

    case RootFUN:
    res = (*root->the.fun.scan)(ss, root->the.fun.p, root->the.fun.s);
    if(res != ResOK)
      return res;
    break;

    case RootREG:
    res = (*root->the.reg.scan)(ss, root->the.reg.thread,
                              root->the.reg.p, root->the.reg.s);
    if(res != ResOK)
      return res;
    break;

    case RootFMT:
    res = (*root->the.fmt.scan)(ss, root->the.fmt.base,
                              root->the.fmt.limit);
    if(res != ResOK)
      return res;
    break;

    default:
    NOTREACHED;
  }

  root->grey = TraceSetDiff(root->grey, ss->traces);

  return ResOK;
}

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Arena RootArena(Root root)
{
  /* Can't AVER root as that would not be thread-safe */
  /* AVERT(Root, root); */
  return root->arena;
}

Res RootDescribe(Root root, mps_lib_FILE *stream)
{
  Res res;

  AVERT(Root, root);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Root $P ($U) {\n", (WriteFP)root, (WriteFU)root->serial,
               "  arena $P ($U)\n", (WriteFP)root->arena, 
               (WriteFU)root->arena->serial,
               "  rank $U\n", (WriteFU)root->rank,
               "  grey $B\n", (WriteFB)root->grey,
               "  summary $B\n", (WriteFB)root->summary,
               NULL);
  if(res != ResOK) return res;

  switch(root->var) {
    case RootTABLE:
    res = WriteF(stream,
                 "  table base $A limit $A\n",
                 root->the.table.base,
                 root->the.table.limit,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootTABLE_MASKED:
    res = WriteF(stream,
                 "  table base $A limit $A mask $B\n",
                 root->the.tableMasked.base,
                 root->the.tableMasked.limit,
                 root->the.tableMasked.mask,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFUN:
    res = WriteF(stream,
                 "  scan function $F\n", (WriteFF)root->the.fun.scan,
                 "  environment p $P s $W\n",
                 root->the.fun.p, (WriteFW)root->the.fun.s,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootREG:
    res = WriteF(stream,
                 "  thread $P\n", (WriteFP)root->the.reg.thread,
                 "  environment p $P", root->the.reg.p,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFMT:
    res = WriteF(stream,
                 "  scan function $F\n", (WriteFF)root->the.fmt.scan,
                 "  format base $A limit $A\n",
                 root->the.fmt.base,
                 root->the.fmt.limit,
                 NULL);
    if(res != ResOK) return res;
    break;
           
    default:
    NOTREACHED;
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (WriteFP)root, (WriteFU)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}

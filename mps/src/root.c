/*  impl.c.root
 *
 *                   ROOT IMPLEMENTATION
 *
 *  $HopeName: MMsrc!root.c(trunk.17) $
 *
 *  Copyright (C) 1995,1996 Harlequin Group, all rights reserved
 *
 *  .scope: This is the implementation of the root datatype.
 *
 *  .design: For design, see design.mps.root and design.mps.root-interface
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(trunk.17) $");

/* .rootcheck: Keep synchonized with impl.h.mpmst.root */
Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Space, root->space); 
  CHECKL(root->serial < root->space->rootSerial);
  CHECKL(RingCheck(&root->spaceRing));
  CHECKL(RankCheck(root->rank));
  CHECKL(TraceSetCheck(root->grey));
  /* Don't need to check var here, because of the switch below */
  switch(root->var)
  {
    case RootTABLE:
    CHECKL(root->the.table.base != 0);
    CHECKL(root->the.table.base < root->the.table.limit);
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

static Res create(Root *rootReturn, Space space,
                  Rank rank, RootVar type,
                  union RootUnion *theUnionP)
{
  Root root;
  Res res;
  void *p;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVERT(Rank, rank);
  AVERT(RootVar, type);

  res = SpaceAlloc(&p, space, sizeof(RootStruct));
  if(res != ResOK)
    return res;
  root = (Root)p; /* Avoid pun */

  root->space = space;
  root->rank = rank;
  root->var = type;
  root->the  = *theUnionP;
  root->grey = TraceSetEMPTY;

  /* See design.mps.space.root-ring */
  RingInit(&root->spaceRing);

  root->serial = space->rootSerial;
  ++space->rootSerial;
  root->sig = RootSig;

  AVERT(Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Space space,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(base != 0);
  AVER(base < limit);  

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return create(rootReturn, space, rank, RootTABLE, &theUnion);
}

Res RootCreateReg(Root *rootReturn, Space space,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVERT(Thread, thread);
  AVER(scan != NULL);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;
  theUnion.reg.s = s;

  return create(rootReturn, space, rank, RootREG, &theUnion);
}

Res RootCreateFmt(Root *rootReturn, Space space,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return create(rootReturn, space, rank, RootFMT, &theUnion);
}

Res RootCreateFun(Root *rootReturn, Space space,
                 Rank rank,
                 RootScanMethod scan,
                 void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVER(RankCheck(rank));
  AVER(FUNCHECK(scan));

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, RootFUN, &theUnion);
}

void RootDestroy(Root root)
{
  Space space;

  AVERT(Root, root);

  space = RootSpace(root);

  AVERT(Space, space);

  RingRemove(&root->spaceRing);
  RingFinish(&root->spaceRing);

  root->sig = SigInvalid;

  SpaceFree(space, (Addr)root, sizeof(RootStruct));
}

Rank RootRank(Root root)
{
  AVERT(Root, root);
  return root->rank;
}

void RootGrey(Root root, TraceId ti)
{
  AVERT(Root, root);
  AVER(TraceIdCheck(ti));
  
  root->grey = TraceSetAdd(root->grey, ti);
}

Res RootScan(ScanState ss, Root root)
{
  Res res;

  AVERT(Root, root);
  AVERT(ScanState, ss);
  AVER(root->rank == ss->rank);

  if(!TraceSetIsMember(root->grey, ss->traceId))
    return ResOK;

  switch(root->var) {
    case RootTABLE:
    TraceScanArea(ss,
      root->the.table.base,
      root->the.table.limit);
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

  root->grey = TraceSetDelete(root->grey, ss->traceId);

  return ResOK;
}

/* Must be thread-safe.  See design.mps.interface.c.thread-safety. */
Space RootSpace(Root root)
{
  return root->space;
}

Res RootDescribe(Root root, mps_lib_FILE *stream)
{
  Res res;

  AVERT(Root, root);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Root $P ($U) {\n", (WriteFP)root, (WriteFU)root->serial,
               "  space $P ($U)\n", (WriteFP)root->space, 
               (WriteFU)root->space->serial,
               "  rank $U\n", (WriteFU)root->rank,
               "  grey $B\n", (WriteFB)root->grey,
               NULL);
  if(res != ResOK) return res;

  switch(root->var)
  {
    case RootTABLE:
    res = WriteF(stream,
                 "  table base $A limit $A\n",
                 root->the.table.base,
                 root->the.table.limit,
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

/*  impl.c.root
 *
 *                   ROOT IMPLEMENTATION
 *
 *  $HopeName: MMsrc!root.c(MMdevel_lib.3) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of roots.
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_lib.3) $");

Bool RootCheck(Root root)
{
  CHECKS(Root, root);
  CHECKU(Space, root->space);
  CHECKL(root->serial < root->space->rootSerial);
  CHECKL(RingCheck(&root->spaceRing));
  CHECKL(RankCheck(root->rank));
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

static Res create(Root *rootReturn, Space space,
                  Rank rank, RootVar type,
                  union RootUnion theUnion)
{
  Root root;
  Res res;

  AVER(rootReturn != NULL);
  AVERT(Space, space);
  AVERT(Rank, rank);

  res = SpaceAlloc((Addr *)&root, space, sizeof(RootStruct));
  if(res != ResOK)
    return res;

  root->space = space;
  root->rank = rank;
  root->var = type;
  root->the  = theUnion;
  root->grey = TraceSetEMPTY;

  RingInit(&root->spaceRing);

  root->sig = RootSig;
  root->serial = space->rootSerial;
  ++space->rootSerial;

  AVERT(Root, root);

  RingAppend(SpaceRootRing(space), &root->spaceRing);

  *rootReturn = root;
  return ResOK;
}

Res RootCreateTable(Root *rootReturn, Space space,
                      Rank rank, Addr *base, Addr *limit)
{
  union RootUnion theUnion;

  AVER(base != 0);
  AVER(base < limit);

  theUnion.table.base = base;
  theUnion.table.limit = limit;

  return create(rootReturn, space, rank, RootTABLE, theUnion);
}

Res RootCreateReg(Root *rootReturn, Space space,
                    Rank rank, Thread thread,
                    RootScanRegMethod scan, void *p)
{
  union RootUnion theUnion;

  AVER(scan != NULL);
  AVERT(Thread, thread);

  theUnion.reg.scan = scan;
  theUnion.reg.thread = thread;
  theUnion.reg.p = p;

  return create(rootReturn, space, rank, RootREG, theUnion);
}

Res RootCreateFmt(Root *rootReturn, Space space,
                  Rank rank, FormatScanMethod scan,
                  Addr base, Addr limit)
{
  union RootUnion theUnion;

  AVER(scan != NULL);
  AVER(base != 0);
  AVER(base < limit);

  theUnion.fmt.scan = scan;
  theUnion.fmt.base = base;
  theUnion.fmt.limit = limit;

  return create(rootReturn, space, rank, RootFMT, theUnion);
}

Res RootCreate(Root *rootReturn, Space space,
                 Rank rank,
                 RootScanMethod scan,
                 void *p, size_t s)
{
  union RootUnion theUnion;

  AVER(scan != NULL);

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, RootFUN, theUnion);
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

void RootGrey(Root root, Space space, TraceId ti)
{
  AVERT(Root, root);
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
      (Addr *)root->the.table.base,
      (Addr *)root->the.table.limit);
    break;

    case RootFUN:
    res = (*root->the.fun.scan)(ss, root->the.fun.p, root->the.fun.s);
    if(res != ResOK)
      return res;
    break;

    case RootREG:
    res = (*root->the.reg.scan)(ss, root->the.reg.thread,
                              root->the.reg.p);
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

/* Must be thread-safe.  See impl.c.mpsi.thread-safety. */
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
               "Root $P ($U) {\n", (void *)root, (unsigned long)root->serial,
               "  space $P ($U)\n", (void *)root->space, (unsigned long)root->space->serial,
               "  rank $U\n", (unsigned long)root->rank,
               "  grey $B\n", (unsigned long)root->grey,
               NULL);
  if(res != ResOK) return res;

  switch(root->var)
  {
    case RootTABLE:
    res = WriteF(stream,
                 "  table base $P limit $P\n",
                 (void *)root->the.table.base,
                 (void *)root->the.table.limit,
                 NULL);
    if(res != ResOK) return res;
    break;

    case RootFUN:
    res = WriteF(stream,
                 "  scan function $P\n", (void *)root->the.fun.scan,
                 "  environment p $P s $W\n",
                 root->the.fun.p, (Word)root->the.fun.s,
                 NULL);
    if(res != ResOK) return res;
    break;

    default:
    NOTREACHED;
  }

  res = WriteF(stream,
               "} Root $P ($U)\n", (void*)root, (unsigned long)root->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}

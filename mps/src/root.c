/*  impl.c.root
 *
 *                   ROOT IMPLEMENTATION
 *
 *  $HopeName: MMsrc!root.c(MMdevel_restr.3) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of roots.
 */

#include "mpm.h"

SRCID(root, "$HopeName: MMsrc!root.c(MMdevel_restr.3) $");

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

Res RootDescribe(Root root, Lib_FILE *stream)
{
  TraceId id;

  AVERT(Root, root);
  AVER(stream != NULL);

  Lib_fprintf(stream,
             "Root %lX {\n"
             "  rank %d\n",
             (unsigned long)root,
             root->rank);

  Lib_fprintf(stream, "  Trace status\n");
  for(id = 0; id < TRACE_MAX; ++id)
    Lib_fprintf(stream, "    %2lu %s\n",
               (unsigned long)id,
               TraceSetIsMember(root->grey, id) ?
                 "grey" : "not grey");

  switch(root->var)
  {
    case RootTABLE:
    Lib_fprintf(stream, "  table base 0x%lX limit 0x%lX\n",
                (unsigned long)root->the.table.base,
                (unsigned long)root->the.table.limit);
    break;

    case RootFUN:
    Lib_fprintf(stream,
                "  scan function 0x%lX\n"
                "  environment p 0x%lX s 0x%lX\n",
                (unsigned long)root->the.fun.scan,
                (unsigned long)root->the.fun.p,
                root->the.fun.s);
    break;

    default:
    NOTREACHED;
  }

  Lib_fprintf(stream, "} Root 0x%lX\n", (unsigned long)root);

  return ResOK;
}
